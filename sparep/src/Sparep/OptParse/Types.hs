{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.OptParse.Types where

import Data.Yaml as Yaml hiding (object)
import GHC.Generics (Generic)
import Path
import Sparep.Card
import YamlParse.Applicative

data Flags
  = Flags
      { flagConfigFile :: Maybe FilePath,
        flagRepetitionDbFile :: Maybe FilePath,
        flagDecks :: [FilePath]
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envConfigFile :: Maybe FilePath,
        envRepetitionDbFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confSpecifications :: [FilePath],
        confRepetitionDbFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalFieldWithDefault
          "specifications"
          []
          "The files and directories containing card definitions"
        <*> optionalField "repetition-database" "The file to store repitition data in"

data Settings
  = Settings
      { setDecks :: [Deck],
        setRepetitionDb :: Path Abs File
      }
  deriving (Show, Eq, Generic)
