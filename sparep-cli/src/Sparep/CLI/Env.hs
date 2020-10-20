{-# LANGUAGE RecordWildCards #-}

module Sparep.CLI.Env where

import Control.Monad.Reader
import Data.Text (Text)
import Servant.Client
import Sparep.CLI.OptParse
import Sparep.Server.Data
import System.Exit

type C a = ReaderT Env IO a

data Env
  = Env
      { envClientEnv :: !(Maybe ClientEnv),
        envUsername :: !(Maybe Username),
        envPassword :: !(Maybe Text)
      }

withClient :: (ClientEnv -> C a) -> C a
withClient func = do
  mCenv <- asks envClientEnv
  case mCenv of
    Nothing -> liftIO $ die "No server configured."
    Just cenv -> func cenv

runClientOrDie :: ClientEnv -> ClientM a -> C a
runClientOrDie cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> liftIO $ die $ show err
    Right res -> pure res

runClient :: ClientEnv -> ClientM a -> C (Either ClientError a)
runClient cenv func = liftIO $ runClientM func cenv

getEnvUsername :: C Username
getEnvUsername = do
  mUsername <- asks envUsername
  case mUsername of
    Nothing -> liftIO $ die "No username configured." -- TODO prompt
    Just un -> pure un

getEnvPassword :: C Text
getEnvPassword = do
  mPassword <- asks envPassword
  case mPassword of
    Nothing -> liftIO $ die "No password configured." -- TODO prompt
    Just pw -> pure pw
