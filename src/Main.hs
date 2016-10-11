{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception.Base (Exception, bracket)
import Control.Monad.Catch (MonadThrow, throwM)
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
import Data.List (intercalate)
import Data.Time (formatTime, defaultTimeLocale)
import Data.Typeable (Typeable)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (StarDict(..), IfoFile(..), Renderer, DataEntry(..), ifoDateFormat)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import Servant (Application, Server, Raw, JSON, Get, Capture, (:>)(..), (:<|>)(..), serve)
import Servant (errBody, err404, throwError)
import System.Console.CmdArgs (Data, cmdArgs, (&=), help, typ, name, explicit)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath.Posix (joinPath, (<.>))
import System.Posix.Syslog (SyslogFn, SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..))
import System.Posix.Syslog (Option(..), withSyslog)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified NLP.Dictionary.StarDict as StarDict (mkDictionary)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 (Markup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.Utils.StaticFiles (serveDirectory)
import System.Posix.Directory (getWorkingDirectory)

data TrSettings = TrSettings {
    tsDictionariesPath :: FilePath
  , tsStaticFilesPath  :: FilePath
  }

data TrState = TrState {
    tsLock         :: MVar ()
  , tsDictionaries :: IORef (Map FilePath StarDict)
  }

data TrException = NoDictionariesPath
  deriving (Eq, Show, Typeable)

instance (Exception TrException)


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


type TemplateAPI = Get '[JSON] [String]
              :<|> Capture "name" String :> Get '[HTML] Markup

serveTemplateAPI :: Map String H.Html -> H.Html -> Server TemplateAPI
serveTemplateAPI templates defaultPage = serveTemplateList :<|> serveTemplate where
  serveTemplateList  = return $ Map.keys templates
  serveTemplate name = return $ fromMaybe defaultPage (Map.lookup name templates)


type DictionaryAPI = Get '[JSON] [FilePath]
                :<|> Capture "name" FilePath :> Get '[JSON] IfoFile
                :<|> Capture "name" FilePath :> Capture "word" Text :> Get '[JSON] [Text]

serveDictionaryAPI :: TrSettings -> TrState -> Server DictionaryAPI
serveDictionaryAPI (TrSettings {..}) (TrState {..}) = serveDictionaryList
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


type TrAPI = Get '[HTML] Markup
        :<|> "static" :> Raw
        :<|> "api" :> "templates" :> TemplateAPI
        :<|> "api" :> "dictionary" :> DictionaryAPI

server :: TrSettings -> TrState -> Server TrAPI
server settings state = return indexPage
                   :<|> serveDirectory (tsStaticFilesPath settings)
                   :<|> serveTemplateAPI templates defaultPage
                   :<|> serveDictionaryAPI settings state where

templates :: Map String H.Html
templates = Map.fromList [
    ("home", homePage)
  ] where
  homePage = "Hello"


defaultPage :: H.Html
defaultPage = "N/A"


indexPage :: H.Html
indexPage = H.docTypeHtml $ do
  H.head $ do
    H.script ! A.src "static/app.js" ! A.type_ "application/javascript;version=1.8" $ ""
  H.body $ do
    H.div ! A.class_ "app" $ "Loading..."


app :: TrSettings -> TrState -> Application
app settings state = serve (Proxy :: Proxy TrAPI) (server settings state)

withEcho :: (SyslogFn -> IO a) -> (SyslogFn -> IO a)
withEcho f = \syslog -> f $ \facility priority message -> do
    syslog facility priority message
    putStrLn . intercalate "::" $ [
        show facility
      , show priority
      , BSC8.unpack message
      ]

data Tr = Tr {
    configFile       :: Maybe FilePath
  , dictionariesPath :: Maybe FilePath
  } deriving (Show, Data, Typeable)


getSettings :: SyslogFn -> IO TrSettings
getSettings syslog = do
  cmd <- cmdArgs Tr {
      configFile = def
        &= explicit &= name "config-file" &= name "c"
        &= help "Configuration file"
        &= typ "PATH"

    , dictionariesPath = def
        &= explicit &= name "dictionaries-path" &= name "d"
        &= help "Directory with dictionaries to serve"
        &= typ "PATH"
    }

  let emptyIni = Ini . HMap.fromList $ []
  ini <- ($ (configFile cmd)) $ maybe
    (return emptyIni)
    (\fn -> readIniFile fn >>= either
      (\err -> do
         syslog DAEMON Error (BSC8.pack $ "Cannot parse ini file: " ++ err)
         return emptyIni)
      (return))
  let fromIni section name = HMap.lookup section (unIni ini) >>= HMap.lookup name

  defStaticFilesPath <- getWorkingDirectory >>= return . joinPath . (:["static"])

  tsDictionariesPath <- fromMaybe (throwM NoDictionariesPath)
    . fmap return . listToMaybe . catMaybes $ [
        T.unpack <$> fromIni "DICTIONARIES" "path"
      , dictionariesPath cmd
      ]

  tsStaticFilesPath <- return $ fromMaybe defStaticFilesPath
    . listToMaybe . catMaybes $ [
          T.unpack <$> fromIni "GLOBAL" "static_files"
        ]

  return $ TrSettings {..}


main :: IO ()
main = withSyslog SyslogConfig {
    identifier = "tr"
  , options = [PID, ODELAY]
  , defaultFacility = DAEMON
  , priorityMask = UpTo Debug
  } $ withEcho $ \syslog -> getSettings syslog >>= main' syslog where

    main' :: SyslogFn -> TrSettings -> IO ()
    main' syslog tsSettings = do
      syslog DAEMON Notice "Started"

      let saSettings = SocketActivationSettings {
          sasPort = Just 8080
        , sasHostPreference = "*4"
        }

      let aqSettings = AutoQuitSettings {
          aqsTimeout = fromIntegral (10 :: Integer)
        , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
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

