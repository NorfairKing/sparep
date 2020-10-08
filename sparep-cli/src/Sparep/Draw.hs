{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Draw where

import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Data.Maybe
import Data.Time
import Sparep.Card
import Sparep.Repetition
import Sparep.State

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
        [ str "Sparep",
          str " ",
          hBox
            [ str "Found ",
              str (show (length menuStateDecks)),
              str " decks containing"
            ],
          hBox
            [ str (show (length (concatMap deckCards menuStateDecks))),
              str " card definitions"
            ],
          hBox
            [ str "which resolve to ",
              str (show (length (concatMap resolveDeck menuStateDecks))),
              str " cards"
            ],
          str " ",
          str "Press enter to study now",
          str " ",
          str "Press d to show decks"
        ]
  ]

drawDecksState :: DecksState -> [Widget ResourceName]
drawDecksState DecksState {..} =
  [ vBox
      [ case decksStateCursor of
          Nothing -> str "No decks"
          Just cursor ->
            padBottom Max $
              let go (Deck {..}, ls) =
                    concat
                      [ [txt $ fromMaybe "No Name" deckName],
                        case ls of
                          Loading ->
                            [ str "Loading"
                            ]
                          Loaded Selection {..} ->
                            [ str (show (length selectionTooSoon)),
                              str (show (length selectionReady)),
                              str (show (length selectionNew))
                            ]
                      ]
               in verticalNonEmptyCursorTableWithHeader go go go [str "Name", str "Done", str "Ready", str "New"] cursor,
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
                go (Card {..}, lTimes) =
                  concat
                    [ [ txt cardFront,
                        txt cardBack
                      ],
                      case lTimes of
                        Loading -> [str "Loading"]
                        Loaded mTimes -> case mTimes of
                          Nothing -> [str " ", str " "]
                          Just (prevTime, nextTime) -> [str $ showTime prevTime, str $ showTime nextTime]
                    ]
             in verticalNonEmptyCursorTableWithHeader go go go [str "Front", str "Back", str "Last Study", str "Next Study"] cursor,
        str "Press Enter to study this deck",
        str "Press Escape to exit"
      ]
  ]

drawStudyState :: StudyState -> [Widget ResourceName]
drawStudyState StudyState {..} =
  [ case studyStateCursor of
      Loading -> centerLayer $ str "Loading cards to study..."
      Loaded mCursor ->
        case mCursor of
          Nothing -> centerLayer $ str "Done"
          Just cursor ->
            let Card {..} = nonEmptyCursorCurrent cursor
             in vBox
                  [ hCenterLayer
                      $ str
                      $ show (length (nonEmptyCursorNext cursor)) ++ " cards left",
                    vCenterLayer $ vBox
                      $ map hCenterLayer
                      $ concat
                        [ [padLeftRight 3 $ txt ins | ins <- maybeToList cardInstructions],
                          [ padAll 1
                              $ border
                              $ vBox
                              $ concat
                                [ [padAll 1 $ txt cardFront],
                                  case studyStateFrontBack of
                                    Front -> []
                                    Back -> [padAll 1 $ txt cardBack]
                                ],
                            padLeftRight 3 $
                              case studyStateFrontBack of
                                Front -> str "Show back: space"
                                Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                          ]
                        ]
                  ]
  ]
