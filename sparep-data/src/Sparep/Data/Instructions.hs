{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Sparep.Data.Instructions where

import Autodocodec
import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Yaml as Yaml
import GHC.Generics (Generic)

newtype Instructions = Instructions {unInstructions :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Instructions)

instance Validity Instructions

instance HasCodec Instructions where
  codec = dimapCodec Instructions unInstructions codec
