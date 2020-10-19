{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sparep.Client.Data.DB where

import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Sparep.Data

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

ClientRepetition
    card CardId
    difficulty Difficulty
    timestamp UTCTime
    deriving Show Eq
|]

clientMakeRepetition :: ClientRepetition -> Repetition
clientMakeRepetition ClientRepetition {..} = Repetition {..}
  where
    repetitionCardId = clientRepetitionCard
    repetitionDifficulty = clientRepetitionDifficulty
    repetitionTimestamp = clientRepetitionTimestamp

makeClientRepetition :: Repetition -> ClientRepetition
makeClientRepetition Repetition {..} = ClientRepetition {..}
  where
    clientRepetitionCard = repetitionCardId
    clientRepetitionDifficulty = repetitionDifficulty
    clientRepetitionTimestamp = repetitionTimestamp
