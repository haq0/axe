{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Axe.Models
import Axe.Util
import Control.Monad (unless, void, when)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Options.Applicative
import System.Console.ANSI
import System.Directory (doesFileExist)
import System.Environment (getEnv)

data Command
  = List ListOpts
  | Create CreateOpts
  | Add AddOpts
  | Delete DeleteOpts
  | Fetch FetchOpts
  | Remove RemoveOpts
  | Clear ClearOpts

data ListOpts = ListOpts
  { listAll :: Bool
  , listFormat :: OutputFormat
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

data FetchOpts = FetchOpts
  { fetchName :: Maybe Text
  , fetchFormat :: OutputFormat
  }

data RemoveOpts = RemoveOpts
  { removeName :: Maybe Text
  , removeFiles :: [FilePath]
  }

data ClearOpts = ClearOpts
  { clearName :: Maybe Text
  , clearForce :: Bool
  }

data OutputFormat
  = Simple
  | Pretty
  | Compact
  deriving (Eq)

formatOpt :: Parser OutputFormat
formatOpt =
  option
    readFormat
    ( long "format"
        <> short 'f'
        <> metavar "FORMAT"
        <> value Pretty
        <> help "Output format (simple|pretty|compact)"
    )
  where
    readFormat = eitherReader $ \case
      "simple" -> Right Simple
      "pretty" -> Right Pretty
      "compact" -> Right Compact
      _ -> Left "Invalid format"

listOpts :: Parser Command
listOpts =
  List
    <$> ( ListOpts
            <$> switch
              ( long "all"
                  <> short 'a'
                  <> help "List all file lists, including empty ones"
              )
            <*> formatOpt
        )

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
                        <> help
                          "Name of the file list to add to (defaults to 'default')"
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

fetchOpts :: Parser Command
fetchOpts =
  Fetch
    <$> ( FetchOpts
            <$> optional
              ( pack
                  <$> strOption
                    ( long "name"
                        <> short 'n'
                        <> metavar "NAME"
                        <> help
                          "Name of the file list to fetch (defaults to 'default')"
                    )
              )
            <*> formatOpt
        )

removeOpts :: Parser Command
removeOpts =
  Remove
    <$> ( RemoveOpts
            <$> optional
              ( pack
                  <$> strOption
                    ( long "name"
                        <> short 'n'
                        <> metavar "NAME"
                        <> help
                          "Name of the file list to remove from (defaults to 'default')"
                    )
              )
            <*> some
              ( argument
                  str
                  ( metavar "FILES..."
                      <> help "Files to remove from the list"
                  )
              )
        )

clearOpts :: Parser Command
clearOpts =
  Clear
    <$> ( ClearOpts
            <$> optional
              ( pack
                  <$> strOption
                    ( long "name"
                        <> short 'n'
                        <> metavar "NAME"
                        <> help
                          "Name of the file list to clear (defaults to 'default')"
                    )
              )
            <*> switch
              ( long "force"
                  <> short 'f'
                  <> help "Force clear without confirmation"
              )
        )

commands :: Parser Command
commands =
  subparser
    ( command
        "list"
        ( info
            (listOpts <**> helper)
            ( progDesc "List all file lists and their contents"
            )
        )
        <> command
          "create"
          ( info
              (createOpts <**> helper)
              (progDesc "Create a new file list")
          )
        <> command
          "add"
          ( info
              (addOpts <**> helper)
              (progDesc "Add files to a list")
          )
        <> command
          "delete"
          ( info
              (deleteOpts <**> helper)
              (progDesc "Delete a file list")
          )
        <> command
          "fetch"
          ( info
              (fetchOpts <**> helper)
              ( progDesc "Fetch contents of a specific file list"
              )
          )
        <> command
          "remove"
          ( info
              (removeOpts <**> helper)
              (progDesc "Remove files from a list")
          )
        <> command
          "clear"
          ( info
              (clearOpts <**> helper)
              (progDesc "Clear all contents from a list")
          )
    )

printFileList :: OutputFormat -> FileList -> IO ()
printFileList format fl = case format of
  Simple -> do
    putStrLn $ unpack (name fl)
    mapM_ putStrLn (files fl)
    putStrLn ""
  Pretty -> do
    setSGR [SetColor Foreground Vivid Blue]
    putStr "📁 "
    setSGR [SetColor Foreground Vivid Green]
    putStrLn $ unpack (name fl)
    setSGR [Reset]
    let
      validFiles = filter (not . null) (files fl)
    if null validFiles
      then putStrLn "   (empty)"
      else
        mapM_ (\f -> putStrLn $ "   └─ " ++ f) validFiles
    putStrLn ""
  Compact -> do
    putStrLn $
      L.intercalate
        " "
        (filter (not . null) $ files fl)

runCommand :: FilePath -> Command -> IO ()
runCommand path = \case
  List opts -> do
    lists <- serializeFromFile path
    let
      filtered =
        if listAll opts
          then lists
          else filter (not . null . files) lists
    mapM_ (printFileList (listFormat opts)) filtered
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
  Fetch opts -> do
    let
      listName = fromMaybe "default" (fetchName opts)
    files <- fetchModelDetails path listName
    case fetchFormat opts of
      Simple -> mapM_ putStrLn files
      Pretty -> do
        setSGR [SetColor Foreground Vivid Blue]
        putStr "📁 "
        setSGR [SetColor Foreground Vivid Green]
        putStrLn $ unpack listName
        setSGR [Reset]
        mapM_ (\f -> putStrLn $ "   └─ " ++ f) files
      Compact -> putStrLn $ L.intercalate " " files
  Remove opts -> do
    let
      listName = fromMaybe "default" (removeName opts)
    lists <- serializeFromFile path
    case filter (\l -> name l == listName) lists of
      [] ->
        putStrLn $
          "List '" ++ unpack listName ++ "' not found"
      [existing] -> do
        let
          filesToRemove = removeFiles opts
          updatedFiles =
            filter (`notElem` filesToRemove) (files existing)
          updatedList = FileList listName updatedFiles
        wrapRemoveFileExistingInstance path listName
        wrapAddFileNewInstance path updatedList
        when
          (length (files existing) == length updatedFiles)
          $ putStrLn
            "Warning: No files were removed (files not found in list)"
      _ -> error "Multiple lists with same name found"
  Clear opts -> do
    let
      listName = fromMaybe "default" (clearName opts)
    lists <- serializeFromFile path
    case filter (\l -> name l == listName) lists of
      [] ->
        putStrLn $
          "List '" ++ unpack listName ++ "' not found"
      [existing] -> do
        when (not $ clearForce opts) $ do
          putStrLn $
            "Are you sure you want to clear all contents from "
              ++ show listName
              ++ "? [y/N]"
          response <- getLine
          unless (response == "y" || response == "Y") $ do
            putStrLn "Clear operation cancelled"
            return ()
        let
          updatedList = FileList listName []
        wrapRemoveFileExistingInstance path listName
        wrapAddFileNewInstance path updatedList
        putStrLn $
          "Cleared all contents from list '"
            ++ unpack listName
            ++ "'"
      _ -> error "Multiple lists with same name found"

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
