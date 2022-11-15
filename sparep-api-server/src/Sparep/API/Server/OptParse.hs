{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Sparep.API.Server.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingPort :: Int,
    settingDbFile :: Path Abs File,
    settingSigningKeyFile :: Path Abs File
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let settingPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc configPort
  settingDbFile <- case flagDbFile <|> envDbFile <|> mc configDbFile of
    Nothing -> resolveFile' "sparep.sqlite3"
    Just dbf -> resolveFile' dbf
  settingSigningKeyFile <- resolveFile' "signing-key.json"
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { configPort :: Maybe Int,
    configDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "port" "The port to serve api requests on" .= configPort
        <*> optionalField "database" "Path to the database" .= configDbFile

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|sparep|])
  resolveFile xdgConfigDir "config.yaml"

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envPort :: Maybe Int,
    envDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SPAREP_API_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "The port to serve api requests on")
      <*> Env.var (fmap Just . Env.str) "DATABASE" (mE <> Env.help "Path to the database file")
  where
    mE = Env.def Nothing

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagPort :: Maybe Int,
    flagDbFile :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                help "The port to serve api requests on"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database",
                help "Path to the database",
                metavar "FILEPATH"
              ]
          )
      )
