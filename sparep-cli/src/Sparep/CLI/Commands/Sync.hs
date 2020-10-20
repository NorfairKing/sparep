module Sparep.CLI.Commands.Sync where

import Data.Appendful.Persistent
import Sparep.CLI.Commands.Import
import Sparep.Client.Data

sync :: C ()
sync = withClient $ \cenv -> withLogin cenv $ \token -> do
  req <- runDB $ clientMakeSyncRequestQuery clientMakeRepetition ClientRepetitionServerId
  resp <- runClientOrDie cenv $ postSync sparepClient token req
  runDB $ clientMergeSyncResponseQuery makeSyncedClientRepetition ClientRepetitionServerId resp
