{-# LANGUAGE LambdaCase #-}

module Axe.Features.Backup () where

import Axe.Models
import Axe.Util (confirmOverwrite)
import Data.Text hiding (map)
import System.Directory
import System.Environment

makeBackupDir ::
  Maybe FilePath -> IO ()
makeBackupDir x =
  case x of
    Just path ->
      doesDirectoryExist path >>= \case
        True -> return ()
        False -> createDirectory path
    Nothing ->
      home >>= \h ->
        doesDirectoryExist
          (h ++ "/.axebak")
          >>= \case
            True -> return ()
            False ->
              createDirectory $ h ++ "/.axebak"
  where
    home :: IO String
    home = getEnv "HOME"

{-
-  Function takes all file lists inside of the config file, and makes individual
-  files for each list rather than just a singular one
-}

copyList ::
  Bool -- confirm overwrite protection?
  -> Maybe FilePath -- path to the new backup location
  -> FileList
  -> IO ()
copyList True (Just fp) fl =
  doesFileExist fp >>= \case
    True ->
      confirmOverwrite (pack fp) >>= \case
        True -> writeFileLists fp [fl]
        False -> return ()
    False -> writeFileLists fp [fl]
copyList True Nothing fl =
  getEnv "HOME" >>= \h ->
    doesFileExist
      ( h
          <> "/.axebak/"
          <> unpack (name fl)
      )
      >>= \case
        True ->
          confirmOverwrite
            ( pack $
                h
                  ++ "/.axebak/"
                  ++ unpack (name fl)
            )
            >>= \case
              True ->
                writeFileLists
                  ( h
                      ++ "/.axebak/"
                      ++ unpack (name fl)
                  )
                  [fl]
              False -> return ()
        False ->
          writeFileLists
            ( h
                ++ "/.axebak/"
                ++ unpack (name fl)
            )
            [fl]
copyList False (Just fp) fl = writeFileLists fp [fl]
