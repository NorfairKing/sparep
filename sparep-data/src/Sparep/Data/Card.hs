{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.Card where

import Data.ByteString (ByteString)
import Data.Text
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Path

data Card
  = Card
      { cardInstructions :: !(Maybe Text),
        cardFront :: !CardSide,
        cardBack :: !CardSide
      }
  deriving (Show, Eq, Generic)

instance Validity Card

data CardSide
  = TextSide Text
  | SoundSide (Path Abs File) ByteString
  deriving (Show, Eq, Generic)

instance Validity CardSide
