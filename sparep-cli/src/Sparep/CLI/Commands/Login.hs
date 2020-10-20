module Sparep.CLI.Commands.Login where

import Sparep.CLI.Commands.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \token ->
  pure ()
