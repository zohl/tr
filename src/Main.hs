{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception.Base (bracket)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.Time (formatTime, defaultTimeLocale)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (StarDict(..), IfoFile(..), Renderer, DataEntry(..), ifoDateFormat)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import Servant (Application, Server, JSON, Get, Capture, (:>)(..), (:<|>)(..), serve)
import Servant (errBody, err404, throwError)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath.Posix (joinPath, (<.>))
import System.Posix.Syslog (SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..))
import System.Posix.Syslog (Option(..), withSyslog)
import qualified Data.ByteString.Lazy.Char8 as BSC8
import qualified Data.Text.Lazy as T
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
 

main :: IO ()
main = withSyslog (SyslogConfig {
    identifier = "tr"
  , options = [PID, ODELAY]
  , defaultFacility = DAEMON
  , priorityMask = UpTo Debug
  }) $ \syslog -> do

    syslog DAEMON Notice "Started" 
        
    let saSettings = SocketActivationSettings {
        sasPort = Just 8080
      , sasHostPreference = "*4"
      }

    let aqSettings = AutoQuitSettings {
        aqsTimeout = fromIntegral (10 :: Integer)
      , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
      }

    dictionariesPath <- joinPath . (:["../references/dictionaries"]) <$> getCurrentDirectory 
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
