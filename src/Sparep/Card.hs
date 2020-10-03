{-# LANGUAGE OverloadedStrings #-}

module Sparep.Card where

import Data.Text (Text)
import Data.Yaml
import Path
import Path.IO
import Text.Show.Pretty
import YamlParse.Applicative

data CardDefs
  = CardDefs
      { cardDefsCards :: [CardDef]
      }
  deriving (Show, Eq)

instance YamlSchema CardDefs where
  yamlSchema =
    objectParser "CardDefs" $
      CardDefs
        <$> optionalFieldWithDefault "cards" [] "Card definitions"

instance FromJSON CardDefs where
  parseJSON = viaYamlSchema

data CardDef
  = CardDef
      { cardDefFront :: Text,
        cardDefBack :: Text
      }
  deriving (Show, Eq)

instance YamlSchema CardDef where
  yamlSchema =
    objectParser "CardDef" $
      CardDef
        <$> requiredField "front" "The front of the card"
        <*> requiredField "back" "The back of the card"

instance FromJSON CardDef where
  parseJSON = viaYamlSchema
