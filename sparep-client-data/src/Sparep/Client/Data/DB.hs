{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Sparep.API.Server.Data
import Sparep.Data

share
  [mkPersist sqlSettings, mkMigrate "clientMigration"]
  [persistLowerCase|

ClientRepetition sql=repetition
    unit StudyUnitId sql=card
    difficulty Difficulty
    timestamp UTCTime
    serverId ServerRepetitionId Maybe
    deriving Show Eq
|]

clientMakeRepetition :: ClientRepetition -> Repetition
clientMakeRepetition ClientRepetition {..} = Repetition {..}
  where
    repetitionUnitId = clientRepetitionUnit
    repetitionDifficulty = clientRepetitionDifficulty
    repetitionTimestamp = clientRepetitionTimestamp

makeSyncedClientRepetition :: ServerRepetitionId -> Repetition -> ClientRepetition
makeSyncedClientRepetition sid = makeClientRepetition (Just sid)

makeUnsyncedClientRepetition :: Repetition -> ClientRepetition
makeUnsyncedClientRepetition = makeClientRepetition Nothing

makeClientRepetition :: Maybe ServerRepetitionId -> Repetition -> ClientRepetition
makeClientRepetition clientRepetitionServerId Repetition {..} = ClientRepetition {..}
  where
    clientRepetitionUnit = repetitionUnitId
    clientRepetitionDifficulty = repetitionDifficulty
    clientRepetitionTimestamp = repetitionTimestamp
