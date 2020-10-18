module Sparep.Client
  ( module Sparep.Client,
    module Sparep.API,
    module X,
  )
where

import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic
import Sparep.API

sparepClient :: SparepRoutes (AsClientT ClientM)
sparepClient = genericClient
