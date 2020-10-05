{-# LANGUAGE LambdaCase #-}

module Sparep.Repetition where

import Control.Monad
import Data.List
import Data.Ord
import Data.Time
import Database.Persist.Sql
import Sparep.Card
import Sparep.DB
import System.Random.Shuffle

-- This computation may take a while, move it to a separate thread with a nice progress bar.
generateStudyDeck :: ConnectionPool -> [Card] -> Int -> IO [Card]
generateStudyDeck pool cards numCards = do
  cardData <-
    forM cards $ \c -> do
      let query =
            map entityVal
              <$> selectList
                [RepetitionCard ==. hashCard c]
                [Desc RepetitionTimestamp]
      (,) c <$> runSqlPool query pool
  now <- getCurrentTime
  shuffleM $ decideStudyDeckSM2 now cardData numCards

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
decideStudyDeckSM0 :: UTCTime -> [(a, [Repetition])] -> Int -> [a]
decideStudyDeckSM0 now cardData numCards =
  let (neverStudied, studiedAtLeastOnce) = partition (null . snd) cardData
      isTooSoon :: (a, [Repetition]) -> Bool
      isTooSoon (_, reps) =
        case sortOn (Down . repetitionTimestamp) (filter ((/= CardIncorrect) . repetitionDifficulty) reps) of
          [] -> False
          (latestRepetition : _) ->
            let i = realToFrac (intervalSize (length reps)) * nominalDay
             in addUTCTime i (repetitionTimestamp latestRepetition) > now
      (_tooSoon, notTooSoon) = partition isTooSoon studiedAtLeastOnce
   in map fst $ chooseFromListsInOrder numCards [notTooSoon, neverStudied]
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
decideStudyDeckSM2 :: UTCTime -> [(a, [Repetition])] -> Int -> [a]
decideStudyDeckSM2 now cardData numCards =
  let (neverStudied, studiedAtLeastOnce) = partition (null . snd) cardData
      isTooSoon :: (a, [Repetition]) -> Bool
      isTooSoon (_, reps) =
        case sortOn (Down . repetitionTimestamp) (filter ((/= CardIncorrect) . repetitionDifficulty) reps) of
          [] -> False
          (latestRepetition : _) ->
            let i = intervalSize reps * nominalDay
             in addUTCTime i (repetitionTimestamp latestRepetition) > now
      (_tooSoon, notTooSoon) = partition isTooSoon studiedAtLeastOnce
   in map fst $ chooseFromListsInOrder numCards [notTooSoon, neverStudied]
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    intervalSize :: [Repetition] -> NominalDiffTime
    intervalSize reps =
      case length reps of
        1 -> 1 * nominalDay
        2 -> 6 * nominalDay
        i -> intervalSize (tail reps) * fromRational (eFactor reps)
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

-- Choose i elements from the lists in order, choosing as many as possible from previous lists.
chooseFromListsInOrder :: Int -> [[a]] -> [a]
chooseFromListsInOrder _ [] = []
chooseFromListsInOrder i _
  | i <= 0 = []
chooseFromListsInOrder i (l : ls) =
  let found = take i l
      j = i - length found
   in found ++ chooseFromListsInOrder j ls
