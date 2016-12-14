{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Monoid((<>))
import Data.Proxy (Proxy(..))
import Data.Text.Lazy (Text)
import Data.List (intercalate)
import Data.Time (formatTime, defaultTimeLocale)
import NLP.Dictionary (Dictionary(..))
import NLP.Dictionary.StarDict (StarDict(..), IfoFile(..), Renderer, DataEntry(..), ifoDateFormat)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import Servant (Application, Server, Raw, JSON, Get, Capture, (:>), (:<|>)(..), serve)
import Servant (errBody, err404, throwError)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (joinPath, (<.>), (</>), takeFileName)
import System.Posix.Syslog (SyslogFn, SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..))
import System.Posix.Syslog (Option(..), withSyslog)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Map.Strict as Map
import qualified NLP.Dictionary.StarDict as StarDict (mkDictionary)
import Servant.Utils.StaticFiles (serveDirectory)
import Common (TrSettings(..))
import Settings (getSettings)
import qualified Data.ByteString.Lazy as BSL
import Data.Typeable (Typeable)
import qualified Network.HTTP.Media as M
import Servant.API (Accept (..), MimeRender (..))

data HTML deriving Typeable

instance Accept HTML where
  contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance MimeRender HTML BSL.ByteString where
  mimeRender _ = id


render :: Renderer
render (UTF8Text s) = "<pre>" <> s <> "</pre>"
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


data TrState = TrState {
    tsLock         :: MVar ()
  , tsDictionaries :: IORef (Map FilePath StarDict)
  }

data CategoryInfo = CategoryInfo {
      ciName       :: FilePath
    , ciDescription :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON CategoryInfo where
  toJSON (CategoryInfo {..}) = object [
      "name"        .= ciName
    , "description" .= ciDescription
    ]

type DictionaryAPI = "categories" :> (
       Get '[JSON] [FilePath]
       -- ^ list of categories
  :<|> Capture "category" FilePath :> (
            Get '[JSON] CategoryInfo
            -- ^ category info
       :<|> Capture "word" Text :> Get '[JSON] [Text]
            -- ^ all translations of the word in the category
       :<|> "dictionaries" :> (
                 Get '[JSON] [FilePath]
                 -- ^ list of all dictionaries in the category
            :<|> Capture "dictionary" FilePath :> (
                      Get '[JSON] IfoFile
                      -- ^ dictionary info
                 :<|> Capture "word" Text :> Get '[JSON] [Text]))))
                      -- ^ translation of the word in the dictionary


serveDictionaryAPI :: TrSettings -> TrState -> Server DictionaryAPI
serveDictionaryAPI (TrSettings {..}) (TrState {..})
     = serveCategoriesList
  :<|> (\cat -> serveCategoryInfo cat
           :<|> (serveTranslation cat Nothing)
           :<|> (serveDictionariesList cat :<|> (\dict ->
                     serveDictionaryInfo cat dict
                :<|> (serveTranslation cat (Just dict))))) where

    serveCategoriesList = liftIO $ getDirectoryContents' tsDictionariesPath
    serveCategoryInfo = undefined

    serveDictionariesList cat = liftIO $ getDirectoryContents' (tsDictionariesPath </> cat)

    serveDictionaryInfo cat dict = withDictionary
      (return . sdIfoFile)
      (tsDictionariesPath </> cat </> dict)

    serveTranslation cat (Just dict) word = withDictionary
      (liftIO . getEntries word)
      (tsDictionariesPath </> cat </> dict)

    serveTranslation cat Nothing word = undefined

    getDirectoryContents' path = filter (\s -> (not . null $ s) && (head s /= '.'))
                        <$> getDirectoryContents path

    withDictionary f path = liftIO (getDictionary path) >>= maybe (dictionaryNotFound path) f

    getDictionary :: FilePath -> IO (Maybe StarDict)
    getDictionary path = bracket (takeMVar tsLock) (putMVar tsLock) $ \_ ->
      readIORef tsDictionaries >>= maybe
      (loadDictionary)
      (return . Just)
      . Map.lookup path where

      loadDictionary :: IO (Maybe StarDict)
      loadDictionary = getDictionariesList >>= \dsl -> case (path `elem` dsl) of
        True -> do
          d <- StarDict.mkDictionary (path </> (takeFileName path) <.> ".ifo") render
          modifyIORef' tsDictionaries (Map.insert path d)
          return $ Just d
        False -> return Nothing

    dictionaryNotFound _path = throwError err404 { errBody = "Dictionary not found" }



type TrAPI = Get '[HTML] BSL.ByteString
        :<|> "static" :> Raw
        :<|> "api" :> DictionaryAPI

server :: TrSettings -> TrState -> BSL.ByteString -> Server TrAPI
server settings state indexPage
     = return indexPage
  :<|> serveDirectory "frontend/static"
  :<|> serveDictionaryAPI settings state where

app :: TrSettings -> TrState -> BSL.ByteString -> Application
app settings state indexPage = serve (Proxy :: Proxy TrAPI) (server settings state indexPage)

withEcho :: (SyslogFn -> IO a) -> (SyslogFn -> IO a)
withEcho f = \syslog -> f $ \facility priority message -> do
    syslog facility priority message
    putStrLn . intercalate "::" $ [
        show facility
      , show priority
      , BSC8.unpack message
      ]

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
      putStrLn $ tsDictionariesPath tsSettings
      let saSettings = SocketActivationSettings {
          sasPort = Just 8080
        , sasHostPreference = "*4"
        }

      let aqSettings = AutoQuitSettings {
          aqsTimeout = Just $ fromIntegral (1200 :: Integer)
        , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
        }

      state <- do
        tsLock         <- newMVar ()
        tsDictionaries <- newIORef Map.empty
        return TrState {..}

      indexPage <- BSL.readFile "frontend/static/index.html"

      withSocketActivation saSettings $
        \sock -> withAutoQuit aqSettings $
          \chan -> runSettingsSocket defaultSettings sock $
             withHeartBeat chan $ app tsSettings state indexPage

      syslog DAEMON Notice "Exiting"

