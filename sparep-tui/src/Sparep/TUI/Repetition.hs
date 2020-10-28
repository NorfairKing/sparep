{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Repetition
  ( generateStudySelection,
    getStudyUnitDates,
    generateStudyDeck,
    Selection (..),
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Time
import Database.Persist.Sql
import Safe
import Sparep.Client.Data
import Sparep.Data
import System.Random.Shuffle

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

-- SM-2, from https://www.supermemo.com/en/archives1990-2015/english/ol/sm2
decideStudyDeckSM2 :: UTCTime -> [(a, [Repetition])] -> Selection a
decideStudyDeckSM2 now cardData =
  let (selectionNew, studiedAtLeastOnce) = partition (null . snd) cardData
      isTooSoon :: (a, [Repetition]) -> Bool
      isTooSoon (_, reps) = maybe False (> now) $ nextRepititionSM2 reps
      (selectionTooSoon, selectionReady) = partition isTooSoon studiedAtLeastOnce
   in fst <$> Selection {..}

-- Nothing means it's new
nextRepititionSM2 :: [Repetition] -> Maybe UTCTime
nextRepititionSM2 reps = do
  let reps' = sortOn repetitionTimestamp reps
  latestRepetition <- lastMay reps'
  is <- intervalSize reps'
  pure $ addUTCTime is (repetitionTimestamp latestRepetition)
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    intervalSize :: [Repetition] -> Maybe NominalDiffTime
    intervalSize reps_ =
      case length $ filter ((/= Incorrect) . repetitionDifficulty) reps_ of
        0 -> Nothing
        1 -> Just $ 1 * nominalDay
        2 -> Just $ 6 * nominalDay
        _ ->
          (* fromRational (eFactor reps_)) <$> (tailMay reps_ >>= intervalSize)
    -- FIXME this is quite inefficient because the eFactor is recalculated O(2) times.
    eFactor :: [Repetition] -> Rational
    eFactor [] = 2.5
    eFactor (r : rs) = max 1.3 $ eFactor rs + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))
      where
        q = difficultyTo0To5Scale (repetitionDifficulty r)
    difficultyTo0To5Scale :: Difficulty -> Rational
    difficultyTo0To5Scale = \case
      Incorrect -> 0
      Hard -> 3
      Good -> 4
      Easy -> 5

data Selection a
  = Selection
      { selectionTooSoon :: [a],
        selectionReady :: [a],
        selectionNew :: [a]
      }
  deriving (Show, Eq, Functor)

studyFromSelection :: Word -> Selection a -> IO [StudyContext a]
studyFromSelection i Selection {..} = do
  readyShuffled <- shuffleM selectionReady
  newShuffled <- shuffleM selectionNew
  shuffleM $
    chooseFromListsInOrder
      i
      [ map (\u -> StudyContext {studyContextNew = False, studyContextUnit = u}) readyShuffled,
        map (\u -> StudyContext {studyContextNew = True, studyContextUnit = u}) newShuffled
      ]

-- Choose i elements from the lists in order, choosing as many as possible from previous lists.
chooseFromListsInOrder :: Word -> [[a]] -> [a]
chooseFromListsInOrder _ [] = []
chooseFromListsInOrder 0 _ = []
chooseFromListsInOrder i (l : ls) =
  let found = take (fromIntegral i) l
      len = genericLength found
      j = i - len
   in if i >= len
        then found ++ chooseFromListsInOrder j ls
        else found
