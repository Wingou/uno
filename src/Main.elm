module Main exposing (Card, CardState(..), Color(..), Game, Model(..), Msg(..), Player, Stack(..), cHeight, cMargin, cOffsetX, cOffsetY, cPlayableUp, cSpriteSize, cStepX, cStepY, cWidth, cardColor, cardPosX, cardPosY, cardSprite, colorY, convertColorToString, discardStackInit, displatBtnEndGame, displayBtnDraw, displayBtnFillDraw, displayBtnPass, displayCard, displayCards, displayHandCards, displayInfoHand, displayPlayer, displayPlayers, displayWinner, drawCardToPlayer, drawStackInit, firstCard, firstPlayer, gameOver, hasWinner, initialModel, isHandPlayable, main, nbCardInHand, nbPointInHand, noCard, noPlayer, notStartedView, omitCard, omitPlayedCard, permutePlayer, playersInit, playingView, tailCard, toPx, update, view)

import Browser
import Debug exposing (log)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, h4, h5, hr, span, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import List exposing (filter, head, isEmpty, length, map, reverse, sortBy, sum, tail)
import String exposing (fromInt)


type CardState
    = Playabled
    | NotPlayabled
    | Seen


type Stack
    = Hand
    | Draw
    | Discard


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | White


type alias Card =
    { value : Int
    , color : Color
    }


type alias Player =
    { id : String
    , name : String
    , hand : List Card
    }


type alias Game =
    { players : List Player
    , drawStack : List Card
    , discardStack : List Card
    , drawing : Bool
    }


type Model
    = NotStarted
    | Playing Game
    | GameOver (List Player)


type Msg
    = RequestedStartGame
    | CardPlayed Card
    | DrawCard
    | DoNothing
    | GameEnded
    | RefillDrawStack
    | Pass


initialModel : Model
initialModel =
    NotStarted


discardStackInit : List Card
discardStackInit =
    []


drawStackInit : List Card
drawStackInit =
    [ { value = 9
      , color = Green
      }
    , { value = 4
      , color = Yellow
      }
    , { value = 3
      , color = Blue
      }
    , { value = 3
      , color = Green
      }
    , { value = 6
      , color = Yellow
      }
    , { value = 9
      , color = Blue
      }
    , { value = 2
      , color = Green
      }
    , { value = 1
      , color = Yellow
      }
    , { value = 7
      , color = Blue
      }
    , { value = 7
      , color = Red
      }
    , { value = 5
      , color = Red
      }
    , { value = 7
      , color = Green
      }
    , { value = 6
      , color = Yellow
      }
    , { value = 9
      , color = Yellow
      }
    , { value = 8
      , color = Green
      }
    , { value = 4
      , color = Blue
      }
    , { value = 5
      , color = Green
      }
    , { value = 3
      , color = Red
      }
    , { value = 4
      , color = Red
      }
    , { value = 6
      , color = Green
      }
    , { value = 1
      , color = Blue
      }
    , { value = 2
      , color = Red
      }
    , { value = 5
      , color = Yellow
      }
    , { value = 6
      , color = Red
      }
    , { value = 8
      , color = Yellow
      }
    , { value = 8
      , color = Green
      }
    , { value = 4
      , color = Green
      }
    , { value = 2
      , color = Blue
      }
    , { value = 1
      , color = Green
      }
    ]


toPx : Int -> String
toPx v =
    fromInt v ++ "px"


cPlayableUp : Int
cPlayableUp =
    13


cStepX : Int
cStepX =
    92


cStepY : Int
cStepY =
    132


cWidth : Int
cWidth =
    82


cHeight : Int
cHeight =
    118


cSpriteSize : Int
cSpriteSize =
    1216


cMargin : Int
cMargin =
    -20


cOffsetX : Int
cOffsetX =
    -15


cOffsetY : Int
cOffsetY =
    -1


cardSprite : String
cardSprite =
    "https://ena.uno/img/ena-sprite.png"


cardPosX : Card -> Int
cardPosX c =
    cOffsetX - c.value * cStepX


cardPosY : Card -> CardState -> Int
cardPosY c cState =
    if cState == Playabled then
        cOffsetY - colorY c.color * cStepY - cPlayableUp

    else
        cOffsetY - colorY c.color * cStepY


displayCard : Card -> CardState -> Html Msg
displayCard c cardState =
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx cWidth)
        , style "height" (toPx (cHeight + cPlayableUp))
        , style "background-position-x" (toPx (cardPosX c))
        , style "background-position-y" (toPx (cardPosY c cardState))
        , style "float" "left"
        , style "background-size" (toPx cSpriteSize)
        , style "margin-left" (toPx cMargin)
        ]
        []


colorY : Color -> Int
colorY color =
    case color of
        Red ->
            0

        Blue ->
            1

        Yellow ->
            2

        Green ->
            3

        _ ->
            -1


cardColor : Color -> String
cardColor c =
    case c of
        Yellow ->
            "Brown"

        _ ->
            convertColorToString c


playersInit : List Player
playersInit =
    [ { id = "0"
      , name = "Wing"
      , hand =
            [ { value = 1
              , color = Red
              }
            , { value = 2
              , color = Yellow
              }
            , { value = 3
              , color = Yellow
              }
            ]
      }
    , { id = "1"
      , name = "Theo"
      , hand =
            [ { value = 5
              , color = Blue
              }
            , { value = 6
              , color = Blue
              }
            , { value = 8
              , color = Red
              }
            ]
      }
    , { id = "2"
      , name = "Alex"
      , hand =
            [ { value = 9
              , color = Red
              }
            , { value = 7
              , color = Yellow
              }
            , { value = 8
              , color = Blue
              }
            ]
      }
    ]


notStartedView =
    div []
        [ br [] []
        , h1 [] [ text "UNO" ]
        , button [ onClick RequestedStartGame ]
            [ h3 []
                [ text "  LET'S START...  "
                ]
            ]
        ]


isHandPlayable : Player -> Card -> Bool
isHandPlayable player masterCard =
    case filter (\h -> h.value == masterCard.value || h.color == masterCard.color) player.hand of
        [] ->
            False

        _ ->
            True


displayBtnDraw : Html Msg
displayBtnDraw =
    button [ onClick DrawCard, style "width" "200px" ] [ text "Draw a card" ]


displayBtnPass : Html Msg
displayBtnPass =
    button [ onClick Pass, style "width" "200px" ] [ text "Pass" ]


displayBtnFillDraw : Html Msg
displayBtnFillDraw =
    button [ onClick RefillDrawStack, style "width" "200px" ] [ text "Fill the deck" ]


displatBtnEndGame : Html Msg
displatBtnEndGame =
    button [ onClick GameEnded, style "width" "200px" ] [ text "End the game" ]


playingView : Game -> Html Msg
playingView game =
    div [ style "background-color" "AZURE" ]
        [ div []
            [ div
                [ style "padding" "2px"
                , style "background-color" "LIGHTBLUE"
                ]
                [ b [ style "font-size" "20px" ] [ text (firstPlayer game.players).name ]
                ]

            -- pour PLAY
            -- il faut Jouable
            , if isHandPlayable (firstPlayer game.players) (firstCard game.discardStack) then
                b [] [ text "Play a card" ]

              else
                b [] [ text "You can't play any card" ]

            -- pour PASS
            -- il faut Piocher
            , if game.drawing then
                div [] [ displayBtnPass ]

              else
                div [] [ text "" ]

            -- pour PIOCHER
            -- il faut Pas Déjà Piocher
            -- il faut Draw plein
            , if not game.drawing && length game.drawStack > 0 then
                div [] [ displayBtnDraw ]

              else
                div [] [ text "" ]

            -- pour FILL
            -- il faut Deck>=2 cartes
            -- il faut Draw vide
            -- il faut Pas déjà Piocher
            , if length game.drawStack == 0 && length game.discardStack > 1 && not game.drawing then
                div [] [ displayBtnFillDraw ]

              else
                div [] [ text "" ]

            -- pour End Game
            -- il faut Pas Jouable
            -- il faut Draw vide
            -- il faut Deck <=1 carte
            -- il faut Pas déjà Piocher
            , if
                not (isHandPlayable (firstPlayer game.players) (firstCard game.discardStack))
                    && length game.drawStack
                    == 0
                    && length game.discardStack
                    <= 1
                    && not game.drawing
              then
                div [] [ displatBtnEndGame ]

              else
                div [] [ text "" ]
            , div [ style "background-color" "LIGHTYELLOW" ]
                [ if length game.drawStack == 0 then
                    h4 []
                        [ text "The deck is empty"
                        , if length game.discardStack <= 1 && not game.drawing then
                            div []
                                [ text "and there's not any card left."
                                ]

                          else
                            text ""
                        ]

                  else
                    h4 [] [ text (fromInt (length game.drawStack)), text " cards left in the deck " ]
                ]
            ]
        , displayPlayers game.players (firstCard game.discardStack) game.drawing
        , div [ style "background-color" "LIGHTYELLOW" ]
            [ h3 [ style "background-color" "YELLOW", style "padding" "10px" ] [ text "GAME BOARD" ]
            , div [ style "overflow" "hidden", style "padding-left" "70px" ] [ displayCard (firstCard game.discardStack) Seen ]
            , div [ style "clear" "left" ] [ displayCards (omitCard (firstCard game.discardStack) game.discardStack) NotPlayabled ]
            ]
        , hr [] []
        , div []
            [ displatBtnEndGame
            , h4 [] [ text "UNO - Workshop ELM - October 2019" ]
            ]
        ]


noPlayer : Player
noPlayer =
    { id = "-1"
    , name = "NoBody"
    , hand = []
    }


noCard : Card
noCard =
    { value = 0
    , color = White
    }


firstPlayer : List Player -> Player
firstPlayer listPlayer =
    case head listPlayer of
        Just player ->
            player

        Nothing ->
            noPlayer


displayInfoHand : List Card -> Card -> Html Msg
displayInfoHand hand masterCard =
    if masterCard == noCard then
        text ((fromInt <| nbPointInHand hand) ++ " points")

    else
        text ((fromInt <| nbCardInHand hand) ++ " cards - " ++ (fromInt <| nbPointInHand hand) ++ " points")


displayPlayers : List Player -> Card -> Bool -> Html Msg
displayPlayers players masterCard drawing =
    div [] (map (\p -> displayPlayer p masterCard (firstPlayer players) drawing) (sortBy .id players))


displayPlayer : Player -> Card -> Player -> Bool -> Html Msg
displayPlayer player masterCard currentPlayer drawing =
    div []
        [ hr [] []
        , div
            [ style "padding" "2px"
            , style "background-color" "LIGHTGREEN"
            ]
            [ b [ style "font-size" "20px" ] [ text player.name ]
            , span [] [ text " - " ]
            , span [] [ displayInfoHand player.hand masterCard ]
            ]
        , div
            [ if currentPlayer == player then
                style "background-color" "GREEN"

              else
                style "background-color" "WHITE"
            ]
            [ br [] []
            , if currentPlayer == player then
                displayHandCards player.hand masterCard drawing

              else
                displayCards player.hand NotPlayabled
            , br [] []
            ]
        ]


nbCardInHand : List Card -> Int
nbCardInHand cards =
    length cards


nbPointInHand : List Card -> Int
nbPointInHand cards =
    sum <| map (\c -> c.value) cards


firstCard : List Card -> Card
firstCard listCard =
    case head listCard of
        Just card ->
            card

        Nothing ->
            noCard


displayHandCards : List Card -> Card -> Bool -> Html Msg
displayHandCards cards masterCard drawing =
    div [ style "overflow" "hidden", style "padding-left" "70px" ]
        (map
            (\c ->
                if c.value == masterCard.value || c.color == masterCard.color then
                    a [ onClick (CardPlayed c) ]
                        [ displayCard c Playabled ]

                else
                    displayCard c NotPlayabled
            )
            cards
        )


displayCards : List Card -> CardState -> Html Msg
displayCards cards cardState =
    div [ style "overflow" "hidden", style "padding-left" "70px" ] (map (\c -> displayCard c cardState) cards)


convertColorToString : Color -> String
convertColorToString color =
    case color of
        Blue ->
            "Blue"

        Red ->
            "Red"

        Green ->
            "Green"

        Yellow ->
            "Yellow"

        White ->
            "White"


displayWinner : List Player -> Html Msg
displayWinner players =
    div []
        [ case
            head
                (sortBy .pts (map (\p -> { name = p.name, pts = nbPointInHand p.hand }) players))
          of
            Just winner ->
                if winner.pts == 0 then
                    text winner.name

                else
                    text (winner.name ++ " with " ++ fromInt winner.pts ++ " points")

            Nothing ->
                text "Nobody wins"
        ]


gameOver : List Player -> Html Msg
gameOver players =
    div []
        [ h1 [] [ text "GAME OVER" ]
        , hr [] []
        , h2 [] [ text "The winner is " ]
        , h2 [] [ displayWinner players ]
        , hr [] []
        , displayPlayers players noCard False
        ]


permutePlayer : List Player -> List Player
permutePlayer listPlayers =
    case tail listPlayers of
        Just tailPlayers ->
            reverse (firstPlayer listPlayers :: reverse tailPlayers)

        Nothing ->
            []


omitCard : Card -> List Card -> List Card
omitCard cardToOmit listCards =
    filter (\c -> c /= cardToOmit) listCards


omitPlayedCard : Card -> List Player -> List Player
omitPlayedCard cardToOmit allPlayers =
    map (\p -> { p | hand = omitCard cardToOmit p.hand }) allPlayers


drawCardToPlayer : List Player -> List Card -> List Player
drawCardToPlayer players cards =
    map
        (\p ->
            { p
                | hand =
                    if (firstPlayer players).id == p.id then
                        reverse (firstCard cards :: reverse p.hand)

                    else
                        p.hand
            }
        )
        players


tailCard : List Card -> List Card
tailCard cards =
    case tail cards of
        Just c ->
            c

        Nothing ->
            []


hasWinner : List Player -> Card -> Bool
hasWinner players lastCardPlayed =
    players
        |> filter (\p -> isEmpty (omitCard lastCardPlayed p.hand))
        |> isEmpty
        |> not


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            notStartedView

        Playing game ->
            playingView game

        GameOver players ->
            gameOver players


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( RequestedStartGame, _ ) ->
            Playing
                { players = playersInit
                , drawStack = omitCard (firstCard drawStackInit) drawStackInit
                , discardStack = firstCard drawStackInit :: []
                , drawing = False
                }

        ( CardPlayed cardPlayed, Playing game ) ->
            if hasWinner game.players cardPlayed then
                GameOver (omitPlayedCard cardPlayed game.players)

            else
                Playing
                    { game
                        | players = permutePlayer <| omitPlayedCard cardPlayed game.players
                        , discardStack = cardPlayed :: game.discardStack
                        , drawing = False
                    }

        ( DrawCard, Playing game ) ->
            Playing
                { game
                    | players = drawCardToPlayer game.players game.drawStack
                    , drawStack = omitCard (firstCard game.drawStack) game.drawStack
                    , drawing = True
                }

        ( RefillDrawStack, Playing game ) ->
            Playing
                { game
                    | drawStack = tailCard game.discardStack
                    , discardStack = firstCard game.discardStack :: []
                }

        ( Pass, Playing game ) ->
            Playing
                { game
                    | players = permutePlayer game.players
                    , drawing = False
                }

        ( DoNothing, _ ) ->
            model

        ( GameEnded, Playing game ) ->
            GameOver game.players

        ( _, _ ) ->
            model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }