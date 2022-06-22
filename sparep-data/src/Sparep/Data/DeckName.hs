{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Sparep.Data.DeckName where

import Autodocodec
import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Yaml as Yaml
import GHC.Generics (Generic)

newtype DeckName = DeckName {unDeckName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec DeckName)

instance Validity DeckName

instance HasCodec DeckName where
  codec = dimapCodec DeckName unDeckName codec
