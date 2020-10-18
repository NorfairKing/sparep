{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Auth.Server
import Servant.Server.Generic
import Sparep.API as API
import Sparep.API.Server.Env
import Sparep.API.Server.Handler

sparepAPIServer :: IO ()
sparepAPIServer = do
  runStderrLoggingT $ withSqlitePool "sparep.sqlite3" 1 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      jwk <- generateKey
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      Warp.run 8000 $ sparepAPIServerApp serverEnv

sparepAPIServerApp :: Env -> Wai.Application
sparepAPIServerApp env =
  genericServeTWithContext
    (flip runReaderT env)
    sparepHandlers
    (sparepContext env)

sparepContext :: Env -> Context '[CookieSettings, JWTSettings]
sparepContext Env {..} = envCookieSettings :. envJWTSettings :. EmptyContext

sparepHandlers :: SparepRoutes (AsServerT H)
sparepHandlers =
  SparepRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin,
      getGreeting = protected handleGetGreeting
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
