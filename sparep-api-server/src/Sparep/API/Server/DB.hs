{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sparep.API.Server.DB where

import Data.Password
import Data.Password.Instances ()
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Sparep.API.Data
import Sparep.Data

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
  name Username
  password PassHash

  UniqueUsername name

  deriving Show Eq


ServerRepetition
  user UserId
  card CardId
  difficulty Difficulty
  time UTCTime

  deriving Show Eq
|]
