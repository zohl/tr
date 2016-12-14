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

import Control.Exception.Base (Exception)
import Data.Typeable (Typeable)

data TrSettings = TrSettings {
    tsDictionariesPath :: FilePath
  , tsReadmeFile       :: FilePath
  }

data TrException = NoDictionariesPath
  deriving (Eq, Show, Typeable)

instance (Exception TrException)

