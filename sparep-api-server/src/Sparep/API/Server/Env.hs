{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server.Env where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Sql
import Servant
import Servant.Auth.Server

type H = ReaderT Env Handler

data Env
  = Env
      { envConnectionPool :: ConnectionPool,
        envCookieSettings :: CookieSettings,
        envJWTSettings :: JWTSettings
      }

runDB :: SqlPersistT IO a -> H a
runDB func = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool func pool
