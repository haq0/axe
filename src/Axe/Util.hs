{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Axe.Util (setupFile, confirmOverwrite) where

import Axe.Models
  ( FileList (..)
  , writeFileLists
  )
import Data.Text
import System.Directory
import System.Environment

setupFile ::
  Maybe FilePath -> IO ()
setupFile (Just path) =
  doesFileExist path >>= \case
    True -> return ()
    False -> do
      let
        list =
          FileList
            { name = "default"
            , files = [""]
            }
      writeFileLists path [list]
setupFile Nothing =
  getEnv "HOME" >>= \env ->
    doesFileExist (env ++ "/.axe.json") >>= \case
      True -> return ()
      False -> do
        let
          list =
            FileList
              { name = "default"
              , files = [""]
              }
        writeFileLists
          (env ++ "/.axe.json")
          [list]

confirmOverwrite :: Text -> IO Bool
confirmOverwrite pwd = do
  putStrLn $
    "Are you sure you want to overwrite "
      <> unpack pwd
      <> "?"
  response <- getLine
  case response of
    "y" -> return True
    "Y" -> return True
    _ -> return False
