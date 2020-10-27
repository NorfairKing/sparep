{-# LANGUAGE RecordWildCards #-}

module Sparep.CLI.Commands.Register where

import Sparep.CLI.Commands.Import

register :: C ()
register = withClient $ \cenv -> do
  registrationFormUsername <- getEnvUsername
  registrationFormPassword <- getEnvPassword
  let rf = RegistrationForm {..}
  NoContent <- runClientOrDie cenv $ postRegister sparepClient rf
  pure ()
