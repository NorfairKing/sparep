{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.OptParse.Types where

import Control.Applicative
import Data.Text (Text)
import Data.Yaml as Yaml hiding (object)
import GHC.Generics (Generic)
import Path
import Sparep.Card
import YamlParse.Applicative

data Flags
  = Flags
      { flagConfigFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envConfigFile :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      {confSpecifications :: [FilePath]}
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalFieldWithDefault "specifications" [] "The files and directories containing card definitions"

data Settings
  = Settings
      { setCardDefs :: [CardDefs]
      }
  deriving (Show, Eq, Generic)
