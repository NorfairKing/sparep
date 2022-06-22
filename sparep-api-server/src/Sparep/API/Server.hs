{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Path
import Servant
import Servant.Auth.Server
import Servant.Server.Generic
import Sparep.API as API
import Sparep.API.Server.Env
import Sparep.API.Server.Handler
import Sparep.API.Server.OptParse
import Sparep.API.Server.SigningKey

sparepAPIServer :: IO ()
sparepAPIServer = do
  Settings {..} <- getSettings
  runStderrLoggingT $
    withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
      runSqlPool (runMigration serverMigration) pool
      liftIO $ do
        jwk <- loadSigningKey settingSigningKeyFile
        let serverEnv =
              Env
                { envConnectionPool = pool,
                  envCookieSettings = defaultCookieSettings,
                  envJWTSettings = defaultJWTSettings jwk
                }
        Warp.run settingPort $ sparepAPIServerApp serverEnv

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
      postSync = protected handlePostSync
    }

protected :: ThrowAll m => (authCookie -> m) -> AuthResult authCookie -> m
protected func (Authenticated authCookie) = func authCookie
protected _ _ = throwAll err401
