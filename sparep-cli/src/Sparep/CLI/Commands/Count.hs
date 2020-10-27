{-# LANGUAGE RecordWildCards #-}

module Sparep.CLI.Commands.Count where

import Sparep.CLI.Commands.Import
import Sparep.Data

count :: CountSettings -> C ()
count CountSettings {..} = do
  cs <- liftIO $ concat <$> mapM resolveRootedDeck countSettingDecks
  sd <- runDB $ generateStudyDeck cs 25 -- TODO make this number configurable
  liftIO $ print $ length sd
