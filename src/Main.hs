{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception.Base (bracket)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Default (Default(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Ini (Ini(..), readIniFile)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, fromJust)
import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.Time (formatTime, defaultTimeLocale)
import Data.Typeable (Typeable)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (StarDict(..), IfoFile(..), Renderer, DataEntry(..), ifoDateFormat)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import Servant (Application, Server, JSON, Get, Capture, (:>)(..), (:<|>)(..), serve)
import Servant (errBody, err404, throwError)
import System.Console.CmdArgs (Data, cmdArgs, (&=), help, typ)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath.Posix (joinPath, (<.>))
import System.Posix.Syslog (SyslogFn, SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..))
import System.Posix.Syslog (Option(..), withSyslog)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified NLP.Dictionary.StarDict as StarDict (mkDictionary)


data TrSettings = TrSettings {
    tsDictionariesPath :: FilePath
  }

data TrState = TrState {
    tsLock         :: MVar ()
  , tsDictionaries :: IORef (Map FilePath StarDict)
  }


render :: Renderer
render (UTF8Text s) = s
render (XDXF s) = s
render _ = error "not supported"

instance ToJSON IfoFile where
  toJSON (IfoFile {..}) = object [
      "version"      .= ifoVersion
    , "bookName"     .= ifoBookName
    , "wordCount"    .= ifoWordCount
    , "synWordCount" .= ifoSynWordCount
    , "author"       .= ifoAuthor
    , "email"        .= ifoEmail
    , "website"      .= ifoWebsite
    , "description"  .= ifoDescription
    , "date"         .= (formatTime defaultTimeLocale ifoDateFormat <$> ifoDate)
    ]

type TrAPI = "dictionary" :> Get '[JSON] [FilePath]
        :<|> "dictionary" :> Capture "name" FilePath :> Get '[JSON] IfoFile
        :<|> "dictionary" :> Capture "name" FilePath :> Capture "word" Text :> Get '[JSON] [Text]

server :: TrSettings -> TrState -> Server TrAPI
server (TrSettings {..}) (TrState {..}) = serveDictionaryList
                                     :<|> serveDictionary
                                     :<|> serveTranslation where

  serveDictionaryList        = liftIO $ getDictionariesList
  serveDictionary            = withDictionary (return . sdIfoFile)
  serveTranslation name word = withDictionary (liftIO . getEntries word) name


  getDictionariesList :: IO [FilePath]
  getDictionariesList = filter (not . flip elem [".", ".."])
                    <$> getDirectoryContents tsDictionariesPath

  getDictionary :: FilePath -> IO (Maybe StarDict)
  getDictionary name = bracket (takeMVar tsLock) (putMVar tsLock) $ \_ ->
    readIORef tsDictionaries >>= maybe
    (loadDictionary)
    (return . Just)
    . Map.lookup name where

    loadDictionary :: IO (Maybe StarDict)
    loadDictionary = getDictionariesList >>= \dsl -> case (name `elem` dsl) of
      True -> do
        d <- StarDict.mkDictionary (joinPath [tsDictionariesPath, name, name <.> ".ifo"]) render
        modifyIORef' tsDictionaries (Map.insert name d)
        return $ Just d
      False -> return Nothing

  dictionaryNotFound _name = throwError err404 { errBody = "Dictionary not found" }

  withDictionary f name = liftIO (getDictionary name) >>= maybe (dictionaryNotFound name) f


app :: TrSettings -> TrState -> Application
app settings state = serve (Proxy :: Proxy TrAPI) (server settings state)


data Tr = Tr {
    config       :: Maybe FilePath
  , dictionaries :: Maybe FilePath
  } deriving (Show, Data, Typeable)


main :: IO ()
main = withSyslog SyslogConfig {
    identifier = "tr"
  , options = [PID, ODELAY]
  , defaultFacility = DAEMON
  , priorityMask = UpTo Debug
  } $ \syslog -> (getArgs syslog) >>= maybe (return ()) (main' syslog) where

    getArgs :: SyslogFn -> IO (Maybe Tr)
    getArgs syslog = cmdArgs Tr {
        config       = def &= help "Configuration file"                   &= typ "PATH"
      , dictionaries = def &= help "Directory with dictionaries to serve" &= typ "PATH"
      } >>= updateFromConfig >>= maybe (return Nothing) check where

        updateFromConfig :: Tr -> IO (Maybe Tr)
        updateFromConfig args = case (config args) of
          Nothing -> return . Just $ args
          Just fn -> (readIniFile fn) >>= either
            (\err -> do
                syslog DAEMON Error (BSC8.pack $ "Cannot parse ini file: " ++ err)
                return Nothing)

            (\ini -> return . Just $ args {
                dictionaries = listToMaybe . catMaybes $ [
                    T.unpack <$> (HMap.lookup "General" (unIni ini) >>= HMap.lookup "Dictionaries")
                  , (dictionaries args)
                  ]
              })

        check :: Tr -> IO (Maybe Tr)
        check args = case (dictionaries args) of
          Nothing -> do
            syslog DAEMON Error "No dictionaries directory was specified"
            return Nothing
          _       -> return . Just $ args


    main' :: SyslogFn -> Tr -> IO ()
    main' syslog args = do
      syslog DAEMON Notice "Started"

      let saSettings = SocketActivationSettings {
          sasPort = Just 8080
        , sasHostPreference = "*4"
        }

      let aqSettings = AutoQuitSettings {
          aqsTimeout = fromIntegral (10 :: Integer)
        , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
        }

      dictionariesPath <- joinPath . (:[fromJust . dictionaries $ args]) <$> getCurrentDirectory
      let tsSettings = TrSettings {
          tsDictionariesPath = dictionariesPath
      }

      state <- do
        tsLock         <- newMVar ()
        tsDictionaries <- newIORef Map.empty
        return TrState {..}

      withSocketActivation saSettings $
        \sock -> withAutoQuit aqSettings $
          \chan -> runSettingsSocket defaultSettings sock $
             withHeartBeat chan $ app tsSettings state

      syslog DAEMON Notice "Exiting"
