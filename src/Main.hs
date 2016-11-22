{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
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
import System.FilePath.Posix (joinPath, (<.>))
import System.Posix.Syslog (SyslogFn, SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..))
import System.Posix.Syslog (Option(..), withSyslog)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Map.Strict as Map
import qualified NLP.Dictionary.StarDict as StarDict (mkDictionary)
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 (Markup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Servant.Utils.StaticFiles (serveDirectory)
import Common (TrSettings(..))
import Settings (getSettings)


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


data TrState = TrState {
    tsLock         :: MVar ()
  , tsDictionaries :: IORef (Map FilePath StarDict)
  }


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
        :<|> "api" :> "dictionary" :> DictionaryAPI

server :: TrSettings -> TrState -> Server TrAPI
server settings state = return indexPage
                   :<|> serveDirectory "frontend/static"
                   :<|> serveDictionaryAPI settings state where

indexPage :: H.Html
indexPage = let
    srcRiotJS = "https://rawgit.com/riot/riot/master/riot+compiler.min.js"
  in H.docTypeHtml $ do
  H.head $ do
    H.link ! A.rel "stylesheet" ! A.href "style.css"
  H.body $ do
    H.preEscapedText "<app>Loading...</app>"
    H.script ! A.src "/static/app.tag" ! A.type_ "riot/tag" $ ""
    H.script ! A.src srcRiotJS $ ""
    H.script $ "riot.mount('app')"


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
          aqsTimeout = fromIntegral (1200 :: Integer)
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

