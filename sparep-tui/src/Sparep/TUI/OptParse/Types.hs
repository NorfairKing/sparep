{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.TUI.OptParse.Types where

import Autodocodec
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Path
import Sparep.Data

data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagRepetitionDbFile :: Maybe FilePath,
    flagDecks :: [FilePath],
    flagCompletionCommand :: Maybe String
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envRepetitionDbFile :: Maybe FilePath,
    envCompletionCommand :: Maybe String
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confSpecifications :: [FilePath],
    confRepetitionDbFile :: Maybe FilePath,
    confCompletionCommand :: Maybe String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldWithDefault "decks" [] "The files and directories containing card definitions" .= confSpecifications
        <*> optionalField "database" "The file to store repitition data in" .= confRepetitionDbFile
        <*> optionalField "completion-command" "The command to run when completing a study session. The number of cards completed is added at the end" .= confCompletionCommand

data Settings = Settings
  { setDecks :: [RootedDeck],
    setRepetitionDb :: Path Abs File,
    setCompletionCommand :: Maybe String
  }
  deriving (Show, Eq, Generic)
