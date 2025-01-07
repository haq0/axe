{-# LANGUAGE OverloadedStrings #-}

module Axe.Models
  ( FileList
  ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text

data FileList = FileList
  { name :: Text
  , files :: [FilePath]
  } deriving (Show)

instance ToJSON FileList where
  toJSON (FileList name files) =
    object
      ["name" .= name, "files" .= files]

instance FromJSON FileList where
  parseJSON (Object v) =
    FileList
      <$> v .: "name"
      <*> v .: "files"
  parseJSON _ = mzero
