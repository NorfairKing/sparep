{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Repetition where

import Control.Monad
import Data.List
import Data.Ord
import Data.Time
import Database.Persist.Sql
import Safe
import Sparep.Card
import Sparep.DB
import System.Random.Shuffle

-- This computation may take a while, move it to a separate thread with a nice progress bar.
generateStudyDeck :: ConnectionPool -> [Card] -> Int -> IO [Card]
generateStudyDeck pool cards numCards =
  shuffleM =<< studyFromSelection numCards <$> generateStudySelection pool cards

generateStudySelection :: ConnectionPool -> [Card] -> IO (Selection Card)
generateStudySelection pool cards = do
  cardData <-
    forM cards $ \c -> do
      let query =
            map entityVal
              <$> selectList
                [RepetitionCard ==. hashCard c]
                [Desc RepetitionTimestamp]
      (,) c <$> runSqlPool query pool
  now <- getCurrentTime
  pure $ decideStudyDeckSM2 now cardData

-- My own algorithm that just takes the least-recently studied cards
decideStudyDeck :: [(a, [Repetition])] -> Int -> [a]
decideStudyDeck cardData numCards =
  let (neverStudied, studiedAtLeastOnce) = partition (null . snd) cardData
      neverStudiedSelected = take numCards neverStudied
      studiedAtLeastOnceSorted =
        sortOn (repetitionTimestamp . head . snd) studiedAtLeastOnce
      studiedAtLeastOnceSelected =
        take (numCards - length neverStudiedSelected) studiedAtLeastOnceSorted
   in map fst $ neverStudiedSelected ++ studiedAtLeastOnceSelected

-- SM-0, from https://www.supermemo.com/en/archives1990-2015/english/ol/beginning
decideStudyDeckSM0 :: UTCTime -> [(a, [Repetition])] -> Selection a
decideStudyDeckSM0 now cardData =
  let (selectionNew, studiedAtLeastOnce) = partition (null . snd) cardData
      isTooSoon :: (a, [Repetition]) -> Bool
      isTooSoon (_, reps) =
        case sortOn (Down . repetitionTimestamp) (filter ((/= CardIncorrect) . repetitionDifficulty) reps) of
          [] -> False
          (latestRepetition : _) ->
            let i = realToFrac (intervalSize (length reps)) * nominalDay
             in addUTCTime i (repetitionTimestamp latestRepetition) > now
      (selectionTooSoon, selectionReady) = partition isTooSoon studiedAtLeastOnce
   in fst <$> Selection {..}
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    -- in number of days
    intervalSize :: Int -> Int
    intervalSize =
      \case
        1 -> 1
        2 -> 7
        3 -> 16
        4 -> 35
        i -> intervalSize (i - 1) * 2

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
  latestRepetition <- lastMay $ sortOn repetitionTimestamp $ filter ((/= CardIncorrect) . repetitionDifficulty) reps
  pure $ addUTCTime (intervalSize reps) (repetitionTimestamp latestRepetition)
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    intervalSize :: [Repetition] -> NominalDiffTime
    intervalSize reps_ =
      case length reps_ of
        1 -> 1 * nominalDay
        2 -> 6 * nominalDay
        _ -> intervalSize (tail reps_) * fromRational (eFactor reps_)
    -- FIXME this is quite inefficient because the eFactor is recalculated O(2) times.
    eFactor :: [Repetition] -> Rational
    eFactor [] = 2.5
    eFactor (r : rs) = max 1.3 $ eFactor rs + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))
      where
        q = difficultyTo0To5Scale (repetitionDifficulty r)
    difficultyTo0To5Scale :: Difficulty -> Rational
    difficultyTo0To5Scale = \case
      CardIncorrect -> 0
      CardHard -> 3
      CardGood -> 4
      CardEasy -> 5

data Selection a
  = Selection
      { selectionTooSoon :: [a],
        selectionReady :: [a],
        selectionNew :: [a]
      }
  deriving (Show, Eq, Functor)

studyFromSelection :: Int -> Selection a -> [a]
studyFromSelection i Selection {..} = chooseFromListsInOrder i [selectionReady, selectionNew]

-- Choose i elements from the lists in order, choosing as many as possible from previous lists.
chooseFromListsInOrder :: Int -> [[a]] -> [a]
chooseFromListsInOrder _ [] = []
chooseFromListsInOrder i _
  | i <= 0 = []
chooseFromListsInOrder i (l : ls) =
  let found = take i l
      j = i - length found
   in found ++ chooseFromListsInOrder j ls
