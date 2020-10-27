{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.DeckName where

import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import YamlParse.Applicative as YamlParse

newtype DeckName = DeckName {unDeckName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity DeckName

instance YamlSchema DeckName where
  yamlSchema = DeckName <$> yamlSchema

instance FromJSON DeckName where
  parseJSON = viaYamlSchema
