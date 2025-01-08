{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Axe.Util (setupFile) where

import Axe.Models (FileList (..), writeFileLists)
import System.Directory
import System.Environment

setupFile :: Maybe FilePath -> IO ()
setupFile (Just path) =
  doesFileExist path >>= \case
    True -> return ()
    False -> do
      let
        list = FileList{name = "default", files = [""]}
      writeFileLists path [list]
setupFile Nothing =
  getEnv "HOME" >>= \env ->
    doesFileExist (env ++ "/.axe.json") >>= \case
      True -> return ()
      False -> do
        let
          list = FileList{name = "default", files = [""]}
        writeFileLists (env ++ "/.axe.json") [list]
