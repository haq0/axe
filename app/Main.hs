{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Axe.Models
import Axe.Util
import Control.Monad (unless, void, when)
import Data.Text (Text, pack)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (getEnv)

data Command
  = List ListOpts
  | Create CreateOpts
  | Add AddOpts
  | Delete DeleteOpts

data ListOpts = ListOpts
  { listAll :: Bool
  }

data CreateOpts = CreateOpts
  { createName :: Text
  , createDesc :: Maybe Text
  , createFiles :: [FilePath]
  }

data AddOpts = AddOpts
  { addName :: Maybe Text
  , addFiles :: [FilePath]
  }

data DeleteOpts = DeleteOpts
  { deleteName :: Text
  , deleteForce :: Bool
  }

-- Parser for list command
listOpts :: Parser Command
listOpts =
  List
    <$> ( ListOpts
            <$> switch
              ( long "all"
                  <> short 'a'
                  <> help "List all file lists, including empty ones"
              )
        )

-- Parser for create command
createOpts :: Parser Command
createOpts =
  Create
    <$> ( CreateOpts
            <$> argument
              (pack <$> str)
              ( metavar "NAME"
                  <> help "Name of the new file list"
              )
            <*> optional
              ( pack
                  <$> strOption
                    ( long "description"
                        <> short 'd'
                        <> metavar "DESC"
                        <> help "Description of the file list"
                    )
              )
            <*> many
              ( argument
                  str
                  ( metavar "FILES..."
                      <> help "Initial files to add"
                  )
              )
        )

-- Parser for add command
addOpts :: Parser Command
addOpts =
  Add
    <$> ( AddOpts
            <$> optional
              ( pack
                  <$> strOption
                    ( long "name"
                        <> short 'n'
                        <> metavar "NAME"
                        <> help "Name of the file list to add to"
                    )
              )
            <*> some
              ( argument
                  str
                  ( metavar "FILES..."
                      <> help "Files to add to the list"
                  )
              )
        )

-- Parser for delete command
deleteOpts :: Parser Command
deleteOpts =
  Delete
    <$> ( DeleteOpts
            <$> argument
              (pack <$> str)
              ( metavar "NAME"
                  <> help "Name of the file list to delete"
              )
            <*> switch
              ( long "force"
                  <> short 'f'
                  <> help "Force deletion without confirmation"
              )
        )

-- Combined command parser
commands :: Parser Command
commands =
  subparser
    ( command
        "list"
        ( info
            listOpts
            ( progDesc "List all file lists and their contents"
            )
        )
        <> command
          "create"
          ( info
              createOpts
              (progDesc "Create a new file list")
          )
        <> command
          "add"
          ( info
              addOpts
              (progDesc "Add files to a list")
          )
        <> command
          "delete"
          ( info
              deleteOpts
              (progDesc "Delete a file list")
          )
    )

-- Main program execution
runCommand :: FilePath -> Command -> IO ()
runCommand path = \case
  List opts -> do
    lists <- serializeFromFile path
    let
      filtered =
        if listAll opts
          then lists
          else filter (not . null . files) lists
    mapM_ print filtered
  Create opts -> do
    let
      newList = FileList (createName opts) (createFiles opts)
    wrapAddFileNewInstance path newList
  Add opts -> do
    let
      listName = maybe "default" id (addName opts)
    lists <- serializeFromFile path
    let
      newList = FileList listName (addFiles opts)
    case filter (\l -> name l == listName) lists of
      [] -> wrapAddFileNewInstance path newList
      [existing] -> do
        let
          updatedList =
            FileList
              listName
              (files existing ++ addFiles opts)
        wrapRemoveFileExistingInstance path listName
        wrapAddFileNewInstance path updatedList
      _ -> error "Multiple lists with same name found"
  Delete opts -> do
    when (not $ deleteForce opts) $ do
      putStrLn $
        "Are you sure you want to delete "
          ++ show (deleteName opts)
          ++ "? [y/N]"
      response <- getLine
      unless (response == "y" || response == "Y") $ do
        putStrLn "Deletion cancelled"
        return ()
    wrapRemoveFileExistingInstance
      path
      (deleteName opts)

main :: IO ()
main = do
  home <- getEnv "HOME"
  let
    defaultPath = home ++ "/.axe.json"
  setupFile Nothing -- Initialize default file if it doesn't exist
  let
    opts =
      info
        (commands <**> helper)
        ( fullDesc
            <> progDesc "Manage lists of files"
            <> header "axe - a file list manager"
        )
  command <- execParser opts
  runCommand defaultPath command
