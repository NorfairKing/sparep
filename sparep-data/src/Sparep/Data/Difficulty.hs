{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.Data.Difficulty where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)

data Difficulty
  = Incorrect
  | Hard
  | Good
  | Easy
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Difficulty)

instance Validity Difficulty

instance HasCodec Difficulty where
  codec = bimapCodec (left T.unpack . parseDifficulty) renderDifficulty codec

renderDifficulty :: Difficulty -> Text
renderDifficulty =
  \case
    Incorrect -> "Incorrect"
    Hard -> "Hard"
    Good -> "Good"
    Easy -> "Easy"

parseDifficulty :: Text -> Either Text Difficulty
parseDifficulty =
  \case
    "Incorrect" -> Right Incorrect
    "Hard" -> Right Hard
    "Good" -> Right Good
    "Correct" -> Right Good
    "Easy" -> Right Easy
    _ -> Left "Unknown Difficulty"

instance PersistField Difficulty where
  toPersistValue = toPersistValue . renderDifficulty

  fromPersistValue = fromPersistValue >=> parseDifficulty

instance PersistFieldSql Difficulty where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
