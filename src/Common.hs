{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Common (
    TrSettings(..)
  , TrException(..)
  ) where

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
  }

data TrException = NoDictionariesPath
  deriving (Eq, Show, Typeable)

instance (Exception TrException)

