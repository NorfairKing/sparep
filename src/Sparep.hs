{-# LANGUAGE OverloadedStrings #-}

module Sparep
  ( sparep,
  )
where

import Data.Text (Text)
import Data.Yaml
import Path
import Path.IO
import Text.Show.Pretty
import YamlParse.Applicative

sparep :: IO ()
sparep = do
  p <- resolveFile' "/home/syd/src/german/vocab/das-geld.yaml"
  r <- readConfigFile p
  pPrint (r :: Maybe CardDefs)

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
