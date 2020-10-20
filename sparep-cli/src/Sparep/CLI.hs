{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.CLI
  ( sparepCLI,
  )
where

import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Path
import Path.IO
import Servant.Client
import Sparep.CLI.Commands
import Sparep.Client.Data
import System.FileLock

sparepCLI :: IO ()
sparepCLI = do
  Instructions disp Settings {..} <- getInstructions
  mCenv <- forM settingBaseUrl $ \burl -> do
    man <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ mkClientEnv man burl
  ensureDir $ parent settingDbFile
  -- Block until locking succeeds
  withFileLock (fromAbsFile settingDbFile ++ ".lock") Exclusive $ \_ ->
    runStderrLoggingT $ withSqlitePool (T.pack (fromAbsFile settingDbFile)) 1 $ \pool -> do
      runSqlPool (runMigration clientMigration) pool
      let env =
            Env
              { envClientEnv = mCenv,
                envUsername = settingUsername,
                envPassword = settingPassword,
                envConnectionPool = pool
              }
      liftIO $ runReaderT (dispatch disp) env

dispatch :: Dispatch -> C ()
dispatch = \case
  DispatchRegister -> register
  DispatchLogin -> login
  DispatchSync -> sync
