{-# LANGUAGE LambdaCase #-}

module Sparep.CLI
  ( sparepCLI,
  )
where

import Sparep.CLI.Commands
import Sparep.CLI.OptParse

sparepCLI :: IO ()
sparepCLI = do
  Instructions disp sets <- getInstructions
  runReaderT (dispatch disp) sets

dispatch :: Dispatch -> C ()
dispatch = \case
  DispatchRegister -> register
  DispatchLogin -> login
  DispatchSync -> sync
