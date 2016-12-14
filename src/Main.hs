{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
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
import qualified Data.Text.Lazy.IO as T

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


data CategoryInfo = CategoryInfo {
      ciName       :: FilePath
    , ciDescription :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON CategoryInfo where
  toJSON (CategoryInfo {..}) = object [
      "name"        .= ciName
    , "description" .= ciDescription
    ]

data TrState = TrState {
    tsLock         :: MVar ()
  , tsCategories   :: IORef (Map FilePath CategoryInfo)
  , tsDictionaries :: IORef (Map FilePath StarDict)
  }


type DictionaryAPI = "categories" :> (
       Get '[JSON] [FilePath]
       -- ^ list of categories
  :<|> Capture "category" FilePath :> (
            Get '[JSON] CategoryInfo
            -- ^ category info
       :<|> "dictionaries" :> (
                 Get '[JSON] [FilePath]
                 -- ^ list of all dictionaries in the category
            :<|> Capture "dictionary" FilePath :> (
                      Get '[JSON] IfoFile
                      -- ^ dictionary info
                 :<|> Capture "word" Text :> Get '[JSON] [Text]))
                      -- ^ translation of the word in the dictionary
       :<|> Capture "word" Text :> Get '[JSON] [Text]))
            -- ^ all translations of the word in the category


serveDictionaryAPI :: TrSettings -> TrState -> Server DictionaryAPI
serveDictionaryAPI (TrSettings {..}) (TrState {..})
     = serveCategoriesList
  :<|> (\cat -> serveCategoryInfo cat
          :<|> (serveDictionariesList cat :<|> (\dict ->
                   serveDictionaryInfo cat dict
              :<|> (serveTranslation cat (Just dict))))
          :<|> (serveTranslation cat Nothing)) where

    serveCategoriesList = liftIO $ getDirectoryContents' tsDictionariesPath
    serveCategoryInfo = undefined

    serveDictionariesList cat = liftIO $ getDirectoryContents' (tsDictionariesPath </> cat)

    serveDictionaryInfo cat dict = withDictionary
      (return . sdIfoFile)
      (tsDictionariesPath </> cat </> dict)

    serveTranslation cat (Just dict) word = withDictionary
      (liftIO . getEntries word)
      (tsDictionariesPath </> cat </> dict)

    serveTranslation cat Nothing word = serveDictionariesList cat
      >>= (fmap concat . mapM (\d -> serveTranslation cat (Just d) word))

    getDirectoryContents' path = filter (\s -> (not . null $ s) && (head s /= '.'))
                        <$> getDirectoryContents path

    withDictionary f path = liftIO (getDictionary path) >>= maybe (dictionaryNotFound path) f

    getCategory :: FilePath -> IO (Maybe CategoryInfo)
    getCategory name = bracket (takeMVar tsLock) (putMVar tsLock) $ \_ ->
      readIORef tsCategories >>= maybe
        (loadCategory)
        (return . Just)
        . Map.lookup name where

      fullPath = tsDictionariesPath </> name

      loadCategory :: IO (Maybe CategoryInfo)
      loadCategory = doesDirectoryExist fullPath >>= \case
        True -> do
          let ciName = name
          let readmeFile = fullPath </> tsReadmeFile
          ciDescription <- doesFileExist readmeFile >>= \case
            True  -> Just <$> T.readFile readmeFile
            False -> return Nothing

          return . Just $ CategoryInfo {..}

        False -> return Nothing




    getDictionary :: FilePath -> IO (Maybe StarDict)
    getDictionary path = bracket (takeMVar tsLock) (putMVar tsLock) $ \_ ->
      readIORef tsDictionaries >>= maybe
      (loadDictionary)
      (return . Just)
      . Map.lookup path where

      fullPath = (path </> (takeFileName path) <.> ".ifo")

      loadDictionary :: IO (Maybe StarDict)
      loadDictionary = doesFileExist fullPath >>= \case
        True -> do
          d <- StarDict.mkDictionary fullPath render
          modifyIORef' tsDictionaries (Map.insert path d)
          return $ Just d
        False -> return Nothing

    dictionaryNotFound path = throwError err404 { errBody = "Dictionary not found" }


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
        tsCategories   <- newIORef Map.empty
        tsDictionaries <- newIORef Map.empty
        return TrState {..}

      indexPage <- BSL.readFile "frontend/static/index.html"

      withSocketActivation saSettings $
        \sock -> withAutoQuit aqSettings $
          \chan -> runSettingsSocket defaultSettings sock $
             withHeartBeat chan $ app tsSettings state indexPage

      syslog DAEMON Notice "Exiting"

