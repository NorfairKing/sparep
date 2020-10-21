{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.Repetition where

import Data.Aeson
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Sparep.Data.CardId
import Sparep.Data.Difficulty

data Repetition
  = Repetition
      { repetitionCardId :: !CardId,
        repetitionDifficulty :: !Difficulty,
        repetitionTimestamp :: !UTCTime
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity Repetition

instance FromJSON Repetition where
  parseJSON = withObject "Repetition" $ \o ->
    Repetition
      <$> o .: "card"
      <*> o .: "difficulty"
      <*> o .: "time"

instance ToJSON Repetition where
  toJSON Repetition {..} =
    object
      [ "card" .= repetitionCardId,
        "difficulty" .= repetitionDifficulty,
        "time" .= repetitionTimestamp
      ]
