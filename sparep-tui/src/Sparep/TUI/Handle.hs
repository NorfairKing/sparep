{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad
import Control.Monad.IO.Class
import Cursor.List.NonEmpty
import qualified Cursor.Simple.List.NonEmpty as Simple
import Cursor.Text
import Cursor.Types
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import Graphics.Vty.Input
import Lens.Micro
import Path
import Sparep.Client.Data
import Sparep.Data
import Sparep.TUI.State
import System.Process.Typed

data Query
  = QueryGetDeckSelection RootedDeck
  | QueryGetDecksSelection [RootedDeck]
  | QueryGetStudyUnitDates (DefinitionContext StudyUnit)
  | QueryGetStudyUnits [RootedDeck] Word

data Response
  = ResponseGetDeckSelection RootedDeck (Selection (DefinitionContext StudyUnit))
  | ResponseGetDecksSelection (Selection (DefinitionContext StudyUnit))
  | ResponseGetStudyUnitDates (DefinitionContext StudyUnit) (Maybe (UTCTime, UTCTime))
  | ResponseGetStudyUnits [StudyContext (DefinitionContext StudyUnit)]

handleTuiEvent ::
  BChan Query -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent qChan s e =
  case s of
    StateMenu ms -> handleMenuEvent qChan ms e
    StateDecks ss -> handleDecksEvent qChan ss e
    StateStudyUnits cs -> handleStudyUnitsEvent qChan cs e
    StateStudy ss -> fmap StateStudy <$> handleStudyEvent ss e

handleMenuEvent ::
  BChan Query -> MenuState -> BrickEvent n Response -> EventM n (Next State)
handleMenuEvent qChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey KEsc [] -> halt $ StateMenu s
        EvKey (KChar 'd') [] -> do
          let decks = menuStateDecks s
          forM_ decks $ \d -> liftIO $ writeBChan qChan $ QueryGetDeckSelection d
          let decksStateCursor = Simple.makeNonEmptyCursor <$> NE.nonEmpty (map (\d -> (d, Loading)) $ sortOn (deckName . rootedDeckDeck) decks)
          continue $ StateDecks DecksState {..}
        EvKey KEnter [] -> handleStudy qChan (menuStateDecks s)
        _ -> continue $ StateMenu s
    AppEvent (ResponseGetDecksSelection sel) -> do
      let s' = s {menuStateSelection = Loaded sel}
      continue $ StateMenu s'
    _ -> continue $ StateMenu s

handleDecksEvent ::
  BChan Query -> DecksState -> BrickEvent n Response -> EventM n (Next State)
handleDecksEvent qChan s e = case decksStateCursor s of
  Nothing ->
    case e of
      VtyEvent vtye ->
        case vtye of
          EvKey KEsc [] -> halt $ StateDecks s
          _ -> continue $ StateDecks s
      _ -> continue $ StateDecks s
  Just cursor ->
    case e of
      VtyEvent vtye ->
        let mDo func =
              let cursor' = fromMaybe cursor $ func cursor
               in continue $ StateDecks $ s {decksStateCursor = Just cursor'}
         in case vtye of
              EvKey KEsc [] -> halt $ StateDecks s
              EvKey KUp [] -> mDo Simple.nonEmptyCursorSelectPrev
              EvKey (KChar 'k') [] -> mDo Simple.nonEmptyCursorSelectPrev
              EvKey KDown [] -> mDo Simple.nonEmptyCursorSelectNext
              EvKey (KChar 'j') [] -> mDo Simple.nonEmptyCursorSelectNext
              EvKey (KChar 'c') [] -> do
                let studyUnitsStateDeck = fst (nonEmptyCursorCurrent cursor)
                studyUnits <- resolveRootedDeck studyUnitsStateDeck
                forM_ studyUnits $ \c -> liftIO $ writeBChan qChan $ QueryGetStudyUnitDates c
                let studyUnitsStateCursor = Simple.makeNonEmptyCursor <$> NE.nonEmpty (map (\c -> (c, Loading)) studyUnits)
                continue $ StateStudyUnits $ StudyUnitsState {..}
              EvKey KEnter [] -> handleStudy qChan [fst (nonEmptyCursorCurrent cursor)]
              _ -> continue $ StateDecks s
      AppEvent (ResponseGetDeckSelection d sel) -> do
        let mCursor' =
              flip fmap (decksStateCursor s) $
                Simple.mapNonEmptyCursor
                  (\t@(d', _) -> if d == d' then (d', Loaded sel) else t)
        let s' = s {decksStateCursor = mCursor'}
        continue $ StateDecks s'
      _ -> continue $ StateDecks s

handleStudyUnitsEvent ::
  BChan Query -> StudyUnitsState -> BrickEvent n Response -> EventM n (Next State)
handleStudyUnitsEvent qChan s e =
  case e of
    VtyEvent vtye ->
      let mDo func =
            let mcursor' = case studyUnitsStateCursor s of
                  Nothing -> Nothing
                  Just cursor -> Just $ fromMaybe cursor $ func cursor
             in continue $ StateStudyUnits $ s {studyUnitsStateCursor = mcursor'}
          tryPlay :: FrontBack -> EventM n (Next State)
          tryPlay fb =
            case studyUnitsStateCursor s of
              Nothing -> continue $ StateStudyUnits s
              Just cursor -> do
                let studyUnit = fst (nonEmptyCursorCurrent cursor)
                case definitionContextUnit studyUnit of
                  CardUnit card -> do
                    let side = case fb of
                          Front -> cardFront card
                          Back -> cardBack card
                    case side of
                      TextSide _ -> pure ()
                      SoundSide fp _ -> playSoundFile fp
                      ImageSide fp _ -> showImageFile fp
                  _ -> pure ()
                continue $ StateStudyUnits s
       in case vtye of
            EvKey KEsc [] -> halt $ StateStudyUnits s
            EvKey KUp [] -> mDo Simple.nonEmptyCursorSelectPrev
            EvKey (KChar 'k') [] -> mDo Simple.nonEmptyCursorSelectPrev
            EvKey KDown [] -> mDo Simple.nonEmptyCursorSelectNext
            EvKey (KChar 'j') [] -> mDo Simple.nonEmptyCursorSelectNext
            EvKey (KChar 'f') [] -> tryPlay Front
            EvKey (KChar 'b') [] -> tryPlay Back
            EvKey KEnter [] -> handleStudy qChan [studyUnitsStateDeck s]
            _ -> continue $ StateStudyUnits s
    AppEvent (ResponseGetStudyUnitDates c dates) -> do
      let mCursor' = flip fmap (studyUnitsStateCursor s) $ Simple.mapNonEmptyCursor (\t@(c', _) -> if c == c' then (c', Loaded dates) else t)
      let s' = s {studyUnitsStateCursor = mCursor'}
      continue $ StateStudyUnits s'
    _ -> continue $ StateStudyUnits s

handleStudyEvent ::
  StudyState ->
  BrickEvent n Response ->
  EventM n (Next StudyState)
handleStudyEvent s e =
  case studyStateCursor s of
    Loading ->
      case e of
        VtyEvent vtye ->
          case vtye of
            EvKey KEsc [] -> halt s
            _ -> continue s
        AppEvent (ResponseGetStudyUnits cs) ->
          continue $
            s
              { studyStateCursor =
                  Loaded $
                    makeNonEmptyCursor (fmap (fmap makeStudyUnitCursor)) <$> NE.nonEmpty cs
              }
        _ -> continue s
    Loaded mCursor ->
      case e of
        VtyEvent vtye ->
          case mCursor of
            Nothing -> halt s
            Just cursor ->
              let doUndo :: EventM n (Next StudyState)
                  doUndo = case studyStateCursor s of
                    Loading -> continue s -- Not loaded yet
                    Loaded mnec -> case mnec of
                      Nothing -> continue s -- No items
                      Just nec -> case nonEmptyCursorSelectPrev (fmap (fmap rebuildStudyUnitCursor)) (fmap (fmap makeStudyUnitCursor)) nec of
                        Nothing -> continue s -- At the start
                        Just nec' -> continue $ s {studyStateCursor = Loaded (Just nec'), studyStateRepetitions = tail $ studyStateRepetitions s}
                  finishStudyUnit :: Difficulty -> EventM n (Next StudyState)
                  finishStudyUnit difficulty = do
                    let cur = nonEmptyCursorCurrent cursor
                    now <- liftIO getCurrentTime
                    let rep =
                          ClientRepetition
                            { clientRepetitionUnit =
                                hashStudyUnit
                                  ( rebuildStudyUnitCursor
                                      ( definitionContextUnit
                                          ( studyContextUnit cur
                                          )
                                      )
                                  ),
                              clientRepetitionDifficulty = difficulty,
                              clientRepetitionTimestamp = now,
                              clientRepetitionServerId = Nothing
                            }
                    let mcursor' = nonEmptyCursorSelectNext (fmap (fmap rebuildStudyUnitCursor)) (fmap (fmap makeStudyUnitCursor)) cursor
                    let mcursor'' =
                          -- Require re-studying incorrect studyUnits
                          if difficulty == Incorrect
                            then Just $ case mcursor' of
                              Nothing -> singletonNonEmptyCursor cur
                              Just cursor' -> nonEmptyCursorAppendAtEnd (fmap rebuildStudyUnitCursor <$> cur) cursor'
                            else mcursor'
                    continue $
                      s
                        { studyStateCursor = Loaded mcursor'',
                          studyStateRepetitions = rep : studyStateRepetitions s
                        }
                  unit = nonEmptyCursorCurrent cursor
                  definitionUnit = studyContextUnit unit
               in case definitionContextUnit definitionUnit of
                    CardUnitCursor cc@CardCursor {..} ->
                      let tryShowExtra :: FrontBack -> EventM n (Next StudyState)
                          tryShowExtra fb = do
                            let side = case fb of
                                  Front -> cardFront cardCursorCard
                                  Back -> cardBack cardCursorCard
                            case side of
                              TextSide _ -> pure ()
                              SoundSide fp _ -> playSoundFile fp
                              ImageSide fp _ -> showImageFile fp
                            continue s
                       in case cardCursorFrontBack of
                            Front ->
                              case vtye of
                                EvKey KEsc [] -> halt s
                                EvKey (KChar 'f') [] -> tryShowExtra Front
                                EvKey (KChar ' ') [] ->
                                  continue $
                                    s
                                      { studyStateCursor =
                                          Loaded $ Just $
                                            cursor & nonEmptyCursorElemL
                                              .~ ( unit
                                                     { studyContextUnit =
                                                         definitionUnit
                                                           { definitionContextUnit =
                                                               CardUnitCursor
                                                                 ( cardCursorShowBack cc
                                                                 )
                                                           }
                                                     }
                                                 )
                                      }
                                EvKey (KChar 'u') [] -> doUndo
                                _ -> continue s
                            Back ->
                              case vtye of
                                EvKey KEsc [] -> halt s
                                EvKey (KChar 'f') [] -> tryShowExtra Front
                                EvKey (KChar 'b') [] -> tryShowExtra Back
                                EvKey (KChar 'i') [] -> finishStudyUnit Incorrect
                                EvKey (KChar 'h') [] -> finishStudyUnit Hard
                                EvKey (KChar 'g') [] -> finishStudyUnit Good
                                EvKey (KChar 'e') [] -> finishStudyUnit Easy
                                _ -> continue s
                    FillExerciseUnitCursor fec@FillExerciseCursor {..} ->
                      let funcDo func =
                            let fec' = fromMaybe fec $ func fec
                             in continue $
                                  s
                                    { studyStateCursor =
                                        Loaded $ Just $
                                          cursor & nonEmptyCursorElemL
                                            .~ ( unit
                                                   { studyContextUnit =
                                                       definitionUnit
                                                         { definitionContextUnit =
                                                             FillExerciseUnitCursor fec'
                                                         }
                                                   }
                                               )
                                    }
                          textDo func =
                            let fec' =
                                  fec
                                    { fillExerciseCursorList =
                                        fillExerciseCursorList
                                          & nonEmptyCursorElemL
                                          %~ ( \pc -> fromMaybe pc $ case pc of
                                                 LitPartCursor _ -> Nothing
                                                 FillPartCursor tc t -> FillPartCursor <$> func tc <*> pure t
                                             )
                                    }
                             in if fillExerciseCursorShow
                                  then continue s
                                  else
                                    continue $
                                      s
                                        { studyStateCursor =
                                            Loaded $ Just $
                                              cursor & nonEmptyCursorElemL
                                                .~ ( unit
                                                       { studyContextUnit =
                                                           definitionUnit
                                                             { definitionContextUnit =
                                                                 FillExerciseUnitCursor fec'
                                                             }
                                                       }
                                                   )
                                        }
                          tryFinishStudyUnit diff =
                            if fillExerciseCursorCorrect fec && not fillExerciseCursorShow
                              then finishStudyUnit diff
                              else continue s
                       in case vtye of
                            EvKey KEsc [] -> halt s
                            EvKey (KChar '\t') [] -> funcDo fillExerciseCursorSeek
                            EvKey KBackTab [] -> funcDo fillExerciseCursorSeekBack
                            EvKey (KChar ' ') [MMeta] -> funcDo $ pure . (\fec_ -> fec_ {fillExerciseCursorShow = True})
                            EvKey (KChar 'i') [MMeta] -> finishStudyUnit Incorrect
                            EvKey (KChar 'h') [MMeta] -> tryFinishStudyUnit Hard
                            EvKey (KChar 'g') [MMeta] -> tryFinishStudyUnit Good
                            EvKey (KChar 'e') [MMeta] -> tryFinishStudyUnit Easy
                            EvKey (KChar 'u') [MMeta] -> doUndo
                            EvKey (KChar c) [] -> textDo $ textCursorInsert c
                            EvKey KBS [] -> textDo $ dullMDelete . textCursorRemove
                            EvKey KDel [] -> textDo $ dullMDelete . textCursorDelete
                            EvKey KUp [] -> textDo $ pure . textCursorSelectStart
                            EvKey KLeft [] -> textDo textCursorSelectPrev
                            EvKey KRight [] -> textDo textCursorSelectNext
                            EvKey KDown [] -> textDo $ pure . textCursorSelectEnd
                            _ -> continue s
        _ -> continue s

-- This computation may take a while, move it to a separate thread with a nice progress bar.
handleStudy :: BChan Query -> [RootedDeck] -> EventM n (Next State)
handleStudy qChan decks = do
  liftIO $ writeBChan qChan $ QueryGetStudyUnits decks 25 -- Make this number configurable
  let studyStateRepetitions = []
  let studyStateCursor = Loading
  continue $ StateStudy $ StudyState {..}

playSoundFile :: Path Abs File -> EventM n ()
playSoundFile p = runProcess_ $ setStdout nullStream $ setStderr nullStream $ proc "play" [fromAbsFile p]

showImageFile :: Path Abs File -> EventM n ()
showImageFile p = runProcess_ $ setStdout nullStream $ setStderr nullStream $ proc "feh" [fromAbsFile p]
