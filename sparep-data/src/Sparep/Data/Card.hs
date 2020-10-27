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

-- | A card is a thing with a front side and a back side.
-- You see the front side and try to remember the back side.
data Card
  = Card
      { cardFront :: !CardSide,
        cardBack :: !CardSide
      }
  deriving (Show, Eq, Generic)

instance Validity Card

data CardSide
  = TextSide !Text
  | SoundSide !(Path Abs File) !ByteString
  | ImageSide !(Path Abs File) !ByteString
  deriving (Show, Eq, Generic)

instance Validity CardSide
