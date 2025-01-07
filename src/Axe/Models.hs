{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Axe.Models
  ( FileList(..)
  , serializeFromFile
  ) where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text hiding (filter)

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

serializeFromFile ::
     FilePath -> IO [FileList]
serializeFromFile path =
  decodeFileStrict path >>= \case
    Just fileList -> return fileList
    Nothing ->
      error "Failed to parse JSON file"

removeItem ::
     Text -> [FileList] -> [FileList]
removeItem a b =
  filter (\b -> name b /= a) b

addItem ::
     FileList
  -> [FileList]
  -> [FileList]
addItem new list = new : list

writeFileLists ::
     FilePath -> [FileList] -> IO ()
writeFileLists path fileLists =
  LB.writeFile path $ encode fileLists
