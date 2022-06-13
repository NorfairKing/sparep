{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Server.Data.DB where

import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Time
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Sparep.API.Server.Data.Password ()
import Sparep.API.Server.Data.Username
import Sparep.Data

share
  [mkPersist sqlSettings, mkMigrate "serverMigration"]
  [persistLowerCase|

User
  name Username
  password (PasswordHash Bcrypt)

  UniqueUsername name

  deriving Show Eq Ord Generic


ServerRepetition sql=repetition
  user UserId
  unit StudyUnitId sql=card
  difficulty Difficulty
  time UTCTime

  deriving Show Eq Ord Generic

|]

instance Validity User

instance Validity ServerRepetition

serverMakeRepetition :: ServerRepetition -> Repetition
serverMakeRepetition ServerRepetition {..} = Repetition {..}
  where
    repetitionUnitId = serverRepetitionUnit
    repetitionDifficulty = serverRepetitionDifficulty
    repetitionTimestamp = serverRepetitionTime

makeServerRepetition :: UserId -> Repetition -> ServerRepetition
makeServerRepetition serverRepetitionUser Repetition {..} = ServerRepetition {..}
  where
    serverRepetitionUnit = repetitionUnitId
    serverRepetitionDifficulty = repetitionDifficulty
    serverRepetitionTime = repetitionTimestamp
