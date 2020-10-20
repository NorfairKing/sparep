{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.CLI
  ( sparepCLI,
  )
where

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Servant.Client
import Sparep.CLI.Commands
import Sparep.CLI.OptParse

sparepCLI :: IO ()
sparepCLI = do
  Instructions disp Settings {..} <- getInstructions
  mCenv <- forM settingBaseUrl $ \burl -> do
    man <- HTTP.newManager HTTP.tlsManagerSettings
    pure $ mkClientEnv man burl
  let env =
        Env
          { envClientEnv = mCenv,
            envUsername = settingUsername,
            envPassword = settingPassword
          }
  runReaderT (dispatch disp) env

dispatch :: Dispatch -> C ()
dispatch = \case
  DispatchRegister -> register
  DispatchLogin -> login
  DispatchSync -> sync
