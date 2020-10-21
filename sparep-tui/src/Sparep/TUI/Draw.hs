{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Path
import Sparep.Data
import Sparep.TUI.Repetition
import Sparep.TUI.State

drawTui :: State -> [Widget ResourceName]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
    StateDecks ds -> drawDecksState ds
    StateCards ds -> drawCardsState ds
    StateStudy ss -> drawStudyState ss

drawMenuState :: MenuState -> [Widget ResourceName]
drawMenuState MenuState {..} =
  [ centerLayer
      $ border
      $ padAll 1
      $ vBox
      $ concat
        [ [ str "Sparep",
            str " "
          ],
          case menuStateSelection of
            Loading -> []
            Loaded Selection {..} ->
              [ str " ",
                str $
                  unwords
                    [ "Done:",
                      show (length selectionTooSoon),
                      " Ready:",
                      show (length selectionReady),
                      " New:",
                      show (length selectionNew)
                    ]
              ],
          [ str " ",
            str "Press enter to study now",
            str " ",
            str "Press d to show decks"
          ]
        ]
  ]

drawDecksState :: DecksState -> [Widget ResourceName]
drawDecksState DecksState {..} =
  [ vBox
      [ case decksStateCursor of
          Nothing -> str "No decks"
          Just cursor ->
            padBottom Max $
              let go (RootedDeck p Deck {..}, ls) =
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
               in verticalNonEmptyCursorTableWithHeader go (map (forceAttr selectedAttr) . go) go [str "Name", str "Done", str "Ready", str "New"] cursor,
        str "Press enter to study the selected deck",
        str "Press c to show the cards in the selected deck"
      ]
  ]

drawCardsState :: CardsState -> [Widget ResourceName]
drawCardsState CardsState {..} =
  [ vBox
      [ padBottom Max $ case cardsStateCursor of
          Nothing -> str "No cards"
          Just cursor ->
            let showTime = formatTime defaultTimeLocale "%F %R"
                go (c@Card {..}, lTimes) =
                  concat
                    [ [ padRight Max $ txt $ renderCardIdHex $ hashCard c
                      ],
                      case lTimes of
                        Loading -> [str "Loading", str "Loading"]
                        Loaded mTimes -> case mTimes of
                          Nothing -> [str " ", str " "]
                          Just (prevTime, nextTime) -> [str $ showTime prevTime, str $ showTime nextTime]
                    ]
             in verticalNonEmptyCursorTableWithHeader go (map (forceAttr selectedAttr) . go) go [str "Id", str "Last Study", str "Next Study"] cursor,
        str "Press Enter to study this deck",
        str "Press Escape to exit"
      ]
  ]

cardSideDescription :: CardSide -> Text
cardSideDescription = \case
  TextSide t -> t
  SoundSide fp _ -> T.pack $ fromRelFile $ filename fp

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
                vCenterLayer $ vBox $
                  map
                    hCenterLayer
                    [ drawCardStudy studyStateFrontBack (nonEmptyCursorCurrent cursor),
                      padLeftRight 3 $
                        case studyStateFrontBack of
                          Front -> str "Show back: space"
                          Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                    ]
              ]
  ]

drawCardStudy :: FrontBack -> Card -> Widget ResourceName
drawCardStudy fb Card {..} =
  vBox $
    concat
      [ [padLeftRight 3 $ txt ins | ins <- maybeToList cardInstructions],
        [ padAll 1
            $ border
            $ vBox
            $ concat
              [ [ padAll 1 $ case cardFront of
                    TextSide t -> txt t
                    SoundSide _ _ -> str "Press 'f' to play sound"
                ],
                case fb of
                  Front -> []
                  Back ->
                    [ padAll 1 $ case cardBack of
                        TextSide t -> txt t
                        SoundSide _ _ -> str "Press 'b' to play sound"
                    ]
              ]
        ]
      ]

selectedAttr :: AttrName
selectedAttr = "selected"
