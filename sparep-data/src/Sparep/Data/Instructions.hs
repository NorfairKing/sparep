{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.Instructions where

import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import YamlParse.Applicative as YamlParse

newtype Instructions = Instructions {unInstructions :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Instructions

instance YamlSchema Instructions where
  yamlSchema = Instructions <$> yamlSchema

instance FromJSON Instructions where
  parseJSON = viaYamlSchema
