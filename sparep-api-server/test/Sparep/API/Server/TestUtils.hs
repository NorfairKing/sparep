{-# LANGUAGE OverloadedStrings #-}

module Sparep.API.Server.TestUtils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp as Warp
import Servant.Auth.Server
import Servant.Client
import Sparep.API.Server
import Sparep.API.Server.DB
import Sparep.API.Server.Env
import Test.Hspec
import Test.Hspec.QuickCheck

serverSpec :: SpecWith ClientEnv -> Spec
serverSpec =
  before (HTTP.newManager defaultManagerSettings) . aroundWith withTestServer
    . modifyMaxSuccess (`div` 20)
    . modifyMaxShrinks (const 0) -- Shrinks are broken when using 'around'

withTestServer :: (ClientEnv -> IO a) -> (HTTP.Manager -> IO a)
withTestServer func man = do
  runNoLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
    void $ runSqlPool (runMigrationQuiet migrateAll) pool
    liftIO $ do
      jwk <- generateKey
      let serverEnv =
            Env
              { envConnectionPool = pool,
                envCookieSettings = defaultCookieSettings,
                envJWTSettings = defaultJWTSettings jwk
              }
      let serverApp = sparepAPIServerApp serverEnv
      testWithApplication (pure serverApp) $ \p -> do
        let env = mkClientEnv man $ BaseUrl Http "127.0.0.1" p ""
        func env

testClientOrErr :: ClientEnv -> ClientM a -> IO a
testClientOrErr cenv func = do
  res <- runClientM func cenv
  case res of
    Left err -> failure $ show err
    Right r -> pure r

testClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClient = flip runClientM

failure :: String -> IO a
failure err = do
  expectationFailure $ show err
  error "Won't get here anyway"
