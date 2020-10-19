{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.Card where

import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
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
