{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-cse #-}

module Settings (
    getSettings
  ) where

import Control.Monad.Catch (throwM)
import Data.Default (Default(..))
import Data.Ini (Ini(..), readIniFile)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Typeable (Typeable)
import System.Console.CmdArgs (Data, cmdArgs, (&=), help, name, explicit, typFile, typDir, program)
import System.Posix.Syslog (SyslogFn, Facility(..), Priority(..))
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import GHC.Generics (Generic)
import Common (TrSettings(..), TrException(..))

data CmdArgs = CmdArgs {
    cmdConfigFile       :: Maybe FilePath
  , cmdDictionariesPath :: Maybe FilePath
  } deriving (Show, Data, Typeable)

data IniArgs = IniArgs {
    iniDictionariesPath :: Maybe FilePath
  } deriving (Show, Generic)

instance Default IniArgs


readIniFileM :: SyslogFn -> FilePath -> IO (Maybe Ini)
readIniFileM syslog fn = readIniFile fn >>= either
  (\err -> do
      syslog DAEMON Error (BSC8.pack $ "Cannot parse ini file: " ++ err)
      return Nothing)
  (return . Just)

getValueM :: (Monad m) => m a -> [Maybe a] -> m a
getValueM mx mxs = fromMaybe mx (fmap return . listToMaybe . catMaybes $ mxs)

getSettings :: SyslogFn -> IO TrSettings
getSettings syslog = do
  (CmdArgs {..}) <- cmdArgs $ CmdArgs {
        cmdConfigFile = def
          &= explicit &= name "config-file" &= name "c"
          &= help "Configuration file"
          &= typFile

      , cmdDictionariesPath = def
          &= explicit &= name "dictionaries" &= name "d"
          &= help "Directory with dictionaries"
          &= typDir
      } &= program "tr"

  (IniArgs {..}) <- case cmdConfigFile of
      Nothing -> return def
      Just fn -> readIniFileM syslog fn >>= \case
        Nothing -> return def
        Just d  -> return IniArgs {
            iniDictionariesPath = fromIni "Dictionaries" "path"
          } where
          fromIni :: T.Text -> T.Text -> Maybe String
          fromIni section key = T.unpack <$> (HMap.lookup section (unIni d) >>= HMap.lookup key)

  dictionariesPath' <- getValueM (throwM NoDictionariesPath) [
      iniDictionariesPath
    , cmdDictionariesPath
    ]

  return TrSettings {
      tsDictionariesPath = dictionariesPath'
    } where

