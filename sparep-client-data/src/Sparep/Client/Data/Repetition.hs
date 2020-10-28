module Sparep.Client.Data.Repetition
  ( generateStudySelection,
    getStudyUnitDates,
    generateStudyDeck,
    Selection (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Database.Persist.Sql
import Safe
import Sparep.Client.Data.DB
import Sparep.Data

getStudyUnitDates :: DefinitionContext StudyUnit -> SqlPersistT IO (Maybe (UTCTime, UTCTime))
getStudyUnitDates unit = do
  reps <-
    map (clientMakeRepetition . entityVal)
      <$> selectList
        [ClientRepetitionUnit ==. hashStudyUnit (definitionContextUnit unit)]
        [Desc ClientRepetitionTimestamp]
  pure $ (,) <$> (repetitionTimestamp <$> headMay reps) <*> nextRepititionSM2 reps

-- This computation may take a while, move it to a separate thread with a nice progress bar.
generateStudyDeck :: [DefinitionContext StudyUnit] -> Word -> SqlPersistT IO [StudyContext (DefinitionContext StudyUnit)]
generateStudyDeck cards numCards =
  generateStudySelection cards >>= (liftIO . studyFromSelection numCards)

generateStudySelection :: [DefinitionContext StudyUnit] -> SqlPersistT IO (Selection (DefinitionContext StudyUnit))
generateStudySelection units = do
  cardData <-
    forM units $ \c ->
      (,) c
        . map (clientMakeRepetition . entityVal)
        <$> selectList
          [ClientRepetitionUnit ==. hashStudyUnit (definitionContextUnit c)]
          [Desc ClientRepetitionTimestamp]
  now <- liftIO getCurrentTime
  pure $ decideStudyDeckSM2 now cardData
