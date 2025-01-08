{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Axe.Models
  ( FileList (..)
  , serializeFromFile
  , addItem
  , writeFileLists
  , wrapAddFileNewInstance
  , wrapRemoveFileExistingInstance
  , fetchAllModelNames
  , fetchModelDetails
  )
where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Text hiding (filter, head, map, take)

data FileList = FileList
  { name :: Text
  , files :: [FilePath]
  }
  deriving (Show)

instance ToJSON FileList where
  toJSON (FileList name files) =
    object ["name" .= name, "files" .= files]

instance FromJSON FileList where
  parseJSON (Object v) =
    FileList <$> v .: "name" <*> v .: "files"
  parseJSON _ = mzero

serializeFromFile :: FilePath -> IO [FileList]
serializeFromFile path =
  decodeFileStrict path >>= \case
    Just fileList -> return fileList
    Nothing -> error "Failed to parse JSON file"

removeItem :: Text -> [FileList] -> [FileList]
removeItem a b = filter (\b -> name b /= a) b

addItem :: FileList -> [FileList] -> [FileList]
addItem new list = new : list

writeFileLists :: FilePath -> [FileList] -> IO ()
writeFileLists path fileLists =
  LB.writeFile path $ encode fileLists

wrapAddFileNewInstance
  :: FilePath -> FileList -> IO ()
wrapAddFileNewInstance path addition =
  serializeFromFile path >>= \existing -> do
    let
      newList = addItem addition existing
    writeFileLists path newList

wrapRemoveFileExistingInstance
  :: FilePath -> Text -> IO ()
wrapRemoveFileExistingInstance path name =
  serializeFromFile path >>= \existing -> do
    let
      newList = removeItem name existing
    writeFileLists path newList

fetchAllModelNames :: FilePath -> IO [Text]
fetchAllModelNames path =
  serializeFromFile path >>= \file -> do
    return $ map name file

fetchModelDetails
  :: FilePath -> Text -> IO [FilePath]
fetchModelDetails path nam =
  serializeFromFile path >>= \file -> do
    let
      model = filter (\b -> name b == nam) file
    case model of
      [m] -> return $ files m
      [] -> return []
      _ -> return $ files (head model)
