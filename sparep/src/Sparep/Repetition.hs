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
  shuffleM $ decideStudyDeckSM0 now cardData numCards

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
            let i = intervalSize (length reps) * nominalDay
             in addUTCTime i (repetitionTimestamp latestRepetition) > now
      (_tooSoon, notTooSoon) = partition isTooSoon studiedAtLeastOnce
   in map fst $ chooseFromListsInOrder numCards [notTooSoon, neverStudied]
  where
    -- How long to wait after the n'th study session before doing the n+1th study session

    intervalSize =
      \case
        1 -> 1
        2 -> 7
        3 -> 16
        4 -> 35
        i -> intervalSize (i - 1) * 2

-- Choose i elements from the lists in order, choosing as many as possible from previous lists.
chooseFromListsInOrder :: Int -> [[a]] -> [a]
chooseFromListsInOrder _ [] = []
chooseFromListsInOrder i _
  | i <= 0 = []
chooseFromListsInOrder i (l : ls) =
  let found = take i l
      j = i - length found
   in found ++ chooseFromListsInOrder j ls
