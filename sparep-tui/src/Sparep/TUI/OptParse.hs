{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Sparep.TUI.OptParse where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import qualified Options.Applicative.Help as OptParse
import Path.IO
import Sparep.CLI.OptParse (getDefaultClientDatabase, getDefaultConfigFile)
import Sparep.Data
import Sparep.TUI.OptParse.Types
import qualified System.Environment as System
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
  decksFromFlags <- mapM (readRootedDeckOrDie <=< resolveFile' <=< normaliseDeckFilePath) flagDecks
  setDecks <-
    (decksFromFlags ++) . concat <$> mapM parseDecks (fromMaybe [] (mc confSpecifications))
  setRepetitionDb <- case flagRepetitionDbFile <|> envRepetitionDbFile <|> mmc confRepetitionDbFile of
    Nothing -> getDefaultClientDatabase
    Just fp -> resolveFile' fp
  let setCompletionCommand = flagCompletionCommand <|> envCompletionCommand <|> mmc confCompletionCommand
  pure Settings {..}
  where
    mc :: (Configuration -> a) -> Maybe a
    mc f = f <$> mConf
    mmc :: (Configuration -> Maybe a) -> Maybe a
    mmc f = mConf >>= f

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> getDefaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SPAREP_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.str) "DATABASE" (mE <> Env.help "The file to store the repetition database in")
      <*> Env.var (fmap Just . Env.str) "COMPLETION_COMMAND" (mE <> Env.help "The command to run when completing a study session. The number of cards completed is added at the end")
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
              [ long "database",
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
    <*> optional
      ( strOption
          ( mconcat
              [ long "completion-command",
                help "The command to run when completing a study session. The number of cards completed is added at the end",
                metavar "COMMAND"
              ]
          )
      )
