{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Draw where

import Brick.AttrMap
import Brick.Markup
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.List.NonEmpty
import qualified Cursor.Simple.List.NonEmpty as Simple
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Sparep.Data
import Sparep.TUI.Repetition
import Sparep.TUI.State

drawTui :: State -> [Widget ResourceName]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
    StateDecks ds -> drawDecksState ds
    StateStudyUnits ds -> drawStudyUnitsState ds
    StateStudy ss -> drawStudyState ss

drawMenuState :: MenuState -> [Widget n]
drawMenuState MenuState {..} =
  [ vBox
      [ centerLayer $ withAttr headingAttr (str "SPAREP"),
        centerLayer
          $ border
          $ padAll 1
          $ vBox
          $ concat
            [ case menuStateSelection of
                Loading -> []
                Loaded Selection {..} ->
                  [ str $
                      unwords
                        [ "Done:",
                          show (length selectionTooSoon),
                          " Ready:",
                          show (length selectionReady),
                          " New:",
                          show (length selectionNew)
                        ]
                  ],
              [ padTop (Pad 1) $ str "Press enter to study now",
                str "Press d to show decks"
              ]
            ]
      ]
  ]

drawDecksState :: DecksState -> [Widget n]
drawDecksState DecksState {..} =
  [ joinBorders $
      vBox
        [ case decksStateCursor of
            Nothing -> str "No decks"
            Just cursor ->
              padBottom Max $
                hBox
                  [ padAll 1 $ drawDeckList cursor,
                    vBorder,
                    padAll 1 $ uncurry drawDeckDetails (nonEmptyCursorCurrent cursor)
                  ],
          hBorder,
          hCenterLayer $
            vBox
              [ str "Press enter to study the selected deck",
                str "Press c to show the cards in the selected deck"
              ]
        ]
  ]

drawDeckList :: Simple.NonEmptyCursor (RootedDeck, Loading (Selection a)) -> Widget n
drawDeckList =
  verticalNonEmptyCursorTableWithHeader
    go
    (map (withAttr selectedAttr) . go)
    go
    ( map
        (withAttr headingAttr)
        [ str "Name",
          str "Done",
          str "Ready",
          str "New"
        ]
    )
  where
    go (RootedDeck _ Deck {..}, ls) =
      concat
        [ [ padRight Max $ txt $ fromMaybe " " deckName
          ],
          case ls of
            Loading ->
              [ str "Loading",
                str "Loading",
                str "Loading"
              ]
            Loaded Selection {..} ->
              [ str (show (length selectionTooSoon)),
                str (show (length selectionReady)),
                str (show (length selectionNew))
              ]
        ]

drawDeckDetails :: RootedDeck -> Loading (Selection a) -> Widget n
drawDeckDetails (RootedDeck _ Deck {..}) ls =
  vBox $
    concat
      [ [ maybe emptyWidget (txtWrap . ("Name: " <>)) deckName,
          maybe emptyWidget (txtWrap . ("Description: " <>)) deckDescription,
          str " "
        ],
        case ls of
          Loading ->
            [ str "Loading",
              str "Loading",
              str "Loading",
              str "Loading"
            ]
          Loaded Selection {..} ->
            [ str ("Total:  " <> show (length selectionTooSoon + length selectionReady + length selectionNew)),
              str ("Done:  " <> show (length selectionTooSoon)),
              str ("Ready: " <> show (length selectionReady)),
              str ("New:   " <> show (length selectionNew))
            ]
      ]

drawStudyUnitsState :: StudyUnitsState -> [Widget n]
drawStudyUnitsState StudyUnitsState {..} =
  [ joinBorders $
      vBox
        [ padBottom Max $
            case studyUnitsStateCursor of
              Nothing -> str "No cards"
              Just cursor ->
                hBox
                  [ padAll 1 $ hLimit 16 $ drawStudyUnitList cursor,
                    vBorder,
                    padAll 1 $ uncurry drawStudyUnitDetails (nonEmptyCursorCurrent cursor)
                  ],
          hBorder,
          hCenterLayer $
            vBox
              [ str "Press Enter to study this deck",
                str "Press Escape to exit"
              ]
        ]
  ]

drawStudyUnitList :: Simple.NonEmptyCursor (StudyUnit, Loading (Maybe (UTCTime, UTCTime))) -> Widget n
drawStudyUnitList =
  verticalNonEmptyCursorTableWithHeader
    go
    (map (withAttr selectedAttr) . go)
    go
    ( map
        (withAttr headingAttr)
        [ str "Id"
        ]
    )
  where
    go (su, _) =
      [ padRight Max $ txt $ renderStudyUnitIdHex $ hashStudyUnit su
      ]

drawStudyUnitDetails :: StudyUnit -> Loading (Maybe (UTCTime, UTCTime)) -> Widget n
drawStudyUnitDetails su lTimes =
  vBox $
    concat
      [ [txt $ "Id: " <> renderStudyUnitIdHex (hashStudyUnit su)],
        case su of
          CardUnit card@Card {..} ->
            concat
              [ [withAttr instructionsAttr $ txtWrap $ "Instructions: " <> ins | ins <- maybeToList cardInstructions],
                [hCenterLayer $ drawCard Back card]
              ],
        case lTimes of
          Loading -> [str "Loading", str "Loading"]
          Loaded mTimes -> case mTimes of
            Nothing -> [str " ", str " "]
            Just (prevTime, nextTime) ->
              [ str $ "Last studied: " <> showTime prevTime,
                str $ "Next study: " <> showTime nextTime
              ]
      ]
  where
    showTime = formatTime defaultTimeLocale "%F %R"

drawStudyState :: StudyState -> [Widget ResourceName]
drawStudyState StudyState {..} =
  [ case studyStateCursor of
      Loading -> centerLayer $ str "Loading cards to study..."
      Loaded mCursor ->
        case mCursor of
          Nothing -> centerLayer $ str "Done"
          Just cursor ->
            vBox
              [ hCenterLayer
                  $ str
                  $ show (length (nonEmptyCursorNext cursor)) ++ " cards left",
                vCenterLayer $ drawStudyUnitCursor (nonEmptyCursorCurrent cursor)
              ]
  ]

drawStudyUnitCursor :: StudyUnitCursor -> Widget ResourceName
drawStudyUnitCursor su = case su of
  CardUnitCursor cc -> drawCardCursor cc
  FillExerciseUnitCursor fec -> drawFillExerciseCursor fec

drawCardCursor :: CardCursor -> Widget n
drawCardCursor CardCursor {..} =
  let Card {..} = cardCursorCard
   in vBox $
        concat
          [ [ hCenterLayer $ padLeftRight 3 $ withAttr instructionsAttr $ txt ins | ins <- maybeToList cardInstructions
            ],
            [ hCenterLayer $ drawCard cardCursorFrontBack cardCursorCard,
              vBox
                [ hCenterLayer $ padLeftRight 3 $
                    case cardCursorFrontBack of
                      Front -> str "Show back: space"
                      Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                ]
            ]
          ]

drawCard :: FrontBack -> Card -> Widget n
drawCard fb Card {..} =
  padAll 1 $ hLimit 40
    $ joinBorders
    $ border
    $ vBox
    $ concat
      [ [ padAll 1 $ drawFrontSide cardFront
        ],
        case fb of
          Front -> []
          Back ->
            [ hBorder,
              padAll 1 $ drawBackSide cardBack
            ]
      ]

drawFrontSide :: CardSide -> Widget n
drawFrontSide = withAttr sideAttr . \case
  TextSide t -> txtWrap t
  SoundSide _ _ -> str "Press 'f' to play sound"

drawBackSide :: CardSide -> Widget n
drawBackSide = withAttr sideAttr . \case
  TextSide t -> txtWrap t
  SoundSide _ _ -> str "Press 'b' to play sound"

drawFillExerciseCursor :: FillExerciseCursor -> Widget ResourceName
drawFillExerciseCursor =
  let partText =
        \case
          LitPart t -> t
          FillPart t -> t
   in foldNonEmptyCursor
        ( \befores current afters ->
            vBox
              [ withAttr sideAttr $ txtWrap $ T.concat (map partText befores),
                withAttr (typingAttr <> sideAttr) $ case current of
                  LitPartCursor t -> txtWrap t
                  FillPartCursor tc _ -> selectedTextCursorWidget TextCursorName tc,
                withAttr sideAttr $ txtWrap $ T.concat (map partText afters)
              ]
        )
        . fillExerciseCursorList

headingAttr :: AttrName
headingAttr = "heading"

selectedAttr :: AttrName
selectedAttr = "selected"

sideAttr :: AttrName
sideAttr = "side"

typingAttr :: AttrName
typingAttr = "typing"

instructionsAttr :: AttrName
instructionsAttr = "instructions"
