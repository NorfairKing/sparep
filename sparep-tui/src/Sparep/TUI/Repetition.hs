{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Repetition where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Ord
import Data.Time
import Database.Persist.Sql
import Safe
import Sparep.Data
import Sparep.TUI.Card
import Sparep.TUI.DB
import System.Random.Shuffle

getCardDates :: Card -> SqlPersistT IO (Maybe (UTCTime, UTCTime))
getCardDates card = do
  reps <- map (clientMakeRepetition . entityVal) <$> selectList [ClientRepetitionCard ==. hashCard card] [Desc ClientRepetitionTimestamp]
  pure $ (,) <$> (repetitionTimestamp <$> headMay reps) <*> nextRepititionSM2 reps

-- This computation may take a while, move it to a separate thread with a nice progress bar.
generateStudyDeck :: [Card] -> Word -> SqlPersistT IO [Card]
generateStudyDeck cards numCards = generateStudySelection cards >>= (liftIO . studyFromSelection numCards)

generateStudySelection :: [Card] -> SqlPersistT IO (Selection Card)
generateStudySelection cards = do
  cardData <-
    forM cards $ \c ->
      (,) c
        . map (clientMakeRepetition . entityVal)
        <$> selectList
          [ClientRepetitionCard ==. hashCard c]
          [Desc ClientRepetitionTimestamp]
  now <- liftIO getCurrentTime
  pure $ decideStudyDeckSM2 now cardData

-- My own algorithm that just takes the least-recently studied cards
decideStudyDeck :: [(a, [Repetition])] -> Word -> [a]
decideStudyDeck cardData numCards =
  let (neverStudied, studiedAtLeastOnce) = partition (null . snd) cardData
      neverStudiedSelected = take (fromIntegral numCards) neverStudied
      studiedAtLeastOnceSorted =
        sortOn (repetitionTimestamp . head . snd) studiedAtLeastOnce
      studiedAtLeastOnceSelected =
        take (fromIntegral numCards - length neverStudiedSelected) studiedAtLeastOnceSorted
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
            let i = realToFrac (intervalSize (genericLength reps)) * nominalDay
             in addUTCTime i (repetitionTimestamp latestRepetition) > now
      (selectionTooSoon, selectionReady) = partition isTooSoon studiedAtLeastOnce
   in fst <$> Selection {..}
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    -- in number of days
    intervalSize :: Word -> Word
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
  let reps' = sortOn repetitionTimestamp reps
  latestRepetition <- lastMay reps'
  is <- intervalSize reps'
  pure $ addUTCTime is (repetitionTimestamp latestRepetition)
  where
    -- How long to wait after the n'th study session before doing the n+1th study session
    intervalSize :: [Repetition] -> Maybe NominalDiffTime
    intervalSize reps_ =
      case length $ filter ((/= CardIncorrect) . repetitionDifficulty) reps_ of
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

studyFromSelection :: Word -> Selection a -> IO [a]
studyFromSelection i Selection {..} = do
  readyShuffled <- shuffleM selectionReady
  newShuffled <- shuffleM selectionNew
  shuffleM $ chooseFromListsInOrder i [readyShuffled, newShuffled]

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
