{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.TUI.OptParse.Types where

import Data.Yaml as Yaml hiding (object)
import GHC.Generics (Generic)
import Path
import Sparep.Data
import YamlParse.Applicative

data Flags
  = Flags
      { flagConfigFile :: Maybe FilePath,
        flagRepetitionDbFile :: Maybe FilePath,
        flagDecks :: [FilePath],
        flagCompletionCommand :: Maybe String
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envConfigFile :: Maybe FilePath,
        envRepetitionDbFile :: Maybe FilePath,
        envCompletionCommand :: Maybe String
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confSpecifications :: [FilePath],
        confRepetitionDbFile :: Maybe FilePath,
        confCompletionCommand :: Maybe String
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalFieldWithDefault "decks" [] "The files and directories containing card definitions"
        <*> optionalField "database" "The file to store repitition data in"
        <*> optionalField "completion-command" "The command to run when completing a study session. The number of cards completed is added at the end"

data Settings
  = Settings
      { setDecks :: [RootedDeck],
        setRepetitionDb :: Path Abs File,
        setCompletionCommand :: Maybe String
      }
  deriving (Show, Eq, Generic)
