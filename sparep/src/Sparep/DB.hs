{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sparep.DB where

import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import Sparep.Card

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Repetition
    card CardId
    difficulty Difficulty
    timestamp UTCTime
    deriving Show
|]
