{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Sparep.TUI.OptParse where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Path
import Path.IO
import Sparep.Data
import Sparep.TUI.OptParse.Types
import qualified System.Directory as FP
import qualified System.Environment as System
import System.Exit
import qualified YamlParse.Applicative as YamlParse

getSettings :: IO Settings
getSettings = do
  flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions flags env config

combineToInstructions ::
  Flags -> Environment -> Maybe Configuration -> IO Settings
combineToInstructions Flags {..} Environment {..} mConf = do
  decksFromFlags <- mapM (readRootedDeckOrDie <=< resolveFile') flagDecks
  setDecks <-
    (decksFromFlags ++) . concat <$> mapM parseDecks (fromMaybe [] (mc confSpecifications))
  setRepetitionDb <-
    case flagRepetitionDbFile <|> envRepetitionDbFile
      <|> mmc confRepetitionDbFile of
      Nothing -> do
        dataDir <- defaultDataDir
        resolveFile dataDir "repetition-data.sqlite3"
      Just fp -> resolveFile' fp
  pure Settings {..}
  where
    mc :: (Configuration -> a) -> Maybe a
    mc f = f <$> mConf
    mmc :: (Configuration -> Maybe a) -> Maybe a
    mmc f = mConf >>= f

parseDecks :: FilePath -> IO [RootedDeck]
parseDecks fp = do
  fileExists <- FP.doesFileExist fp
  if fileExists
    then do
      p <- resolveFile' fp
      maybeToList <$> readRootedDeck p
    else do
      dirExists <- FP.doesDirectoryExist fp
      if dirExists
        then do
          p <- resolveDir' fp
          fs <- snd <$> listDirRecur p
          fmap catMaybes
            $ forM fs
            $ \f -> do
              errOrDefs <- readRootedDeckOrErr f
              pure $ either (const Nothing) Just =<< errOrDefs
        else pure []

readDeckOrDie :: Path Abs File -> IO Deck
readDeckOrDie p = do
  md <- YamlParse.readConfigFile p
  case md of
    Nothing -> die $ "Deck file not found: " <> fromAbsFile p
    Just d -> pure d

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|sparep|])
  resolveFile xdgConfigDir "config.yaml"

defaultDataDir :: IO (Path Abs Dir)
defaultDataDir = getXdgDir XdgData (Just [reldir|sparep|])

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SPAREP_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var
        (fmap Just . Env.str)
        "REPETITION_DATABASE"
        (mE <> Env.help "The file to store the repetition database in")
  where
    mE = Env.def Nothing

getArguments :: IO Flags
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Flags
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ :: ParserPrefs
    prefs_ =
      defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Flags
argParser =
  info
    (helper <*> parseArgs)
    (fullDesc <> footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration),
          "",
          "Deck file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Deck)
        ]

parseArgs :: Parser Flags
parseArgs = parseFlags

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "the config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "repetition-database",
                help "The path to store the repetition database in",
                metavar "FILEPATH"
              ]
          )
      )
    <*> many
      ( strOption
          ( mconcat
              [ long "deck",
                help "A path to more decks",
                metavar "PATH"
              ]
          )
      )
