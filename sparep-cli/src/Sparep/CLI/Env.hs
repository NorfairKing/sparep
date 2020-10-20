module Sparep.CLI.Env where

import Control.Monad.Reader
import Sparep.CLI.OptParse

type C a = ReaderT Settings IO a
