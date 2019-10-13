module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, span, br, h3, h4, hr, a)
import Html.Events exposing (onClick)
import String exposing (fromInt)
import List exposing (map, head, tail, reverse, filter)
import Debug exposing (log)


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
    { currentPlayer : Player
    , currentCardPlayed : Card
    , players : List Player
    , drawStack : List Card
    , discardStack : List Card
    }


type Model
    = NotStarted
    | Playing Game
    | GameOver Player


type Msg
    = RequestedStartGame
    | CardPlayed Card

initialModel : Model
initialModel =
    NotStarted

discardStackInit : List Card
discardStackInit = []

drawStackInit : List Card
drawStackInit =
    [ { value = 9
      , color = Green
      }
    , { value = 7
      , color = Yellow
      }
    , { value = 5
      , color = Blue
      }
    , { value = 3
      , color = Green
      }
    , { value = 4
      , color = Yellow
      }
    , { value = 8
      , color = Blue
      }
    , { value = 2
      , color = Green
      }
    , { value = 1
      , color = Yellow
      }
    , { value = 9
      , color = Blue
      }
    ]


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
              , color = Green
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
    button [ onClick RequestedStartGame ] [ text "Press to start ..." ]


playingView : Game -> Html Msg
playingView game =
    div []
        [ h3 [] [ text "players : ", text (firstPlayer game.players).name ]
        , hr [] <| map (\player -> displayPlayer player) game.players
        , h3 [] [ text "drawStack" ]
        , hr [] <| map (\drawCard -> displayCard drawCard) game.drawStack
        , h3 [] [ text "discardStack" ]
        , hr [] <| map (\discardCard -> displayCard discardCard) game.discardStack
        ]

noPlayer : Player
noPlayer =  { id = "-1"
            , name = "NoBody"
            , hand = []
            }

noCard : Card
noCard =  { value = 0,
            color= White
          }


firstPlayer : List Player -> Player
firstPlayer listPlayer =
    case head listPlayer of
        Just player ->
            player

        Nothing ->
            noPlayer

displayCard : Card -> Html Msg
displayCard c =
        a[
            onClick (CardPlayed c)
        ][
                text ("[" ++ fromInt c.value ++ "-" ++ convertColorToString c.color ++ "]")
        ]


displayCards : List Card -> Html Msg
displayCards cards =
    div []  (map (\c -> displayCard c) cards)


displayPlayer : Player -> Html Msg
displayPlayer player =
    div [] [ text player.name, displayCards player.hand ]

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

gameOver =
    div []
        [ h3 [] [ text "GameOver" ]
        , h4 [] [ text "TOTO is the winner !" ]
        ]


permutePlayer : List Player -> List Player
permutePlayer listPlayers =
    case (tail listPlayers) of
        Just tailPlayers ->
             reverse ((firstPlayer listPlayers) ::  reverse tailPlayers)
             
        Nothing ->
            []

omitCard: Card -> List Card -> List Card 
omitCard cardToOmit oneHand =
    filter (\c -> c/=cardToOmit ) oneHand



omitPlayedCard : Card -> List Player -> List Player
omitPlayedCard cardToOmit allPlayers =
  map (\p -> { p | hand=omitCard cardToOmit p.hand} )  allPlayers 


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            notStartedView

        Playing game ->
            playingView game

        _ ->
            gameOver

updateModel : Card -> Model -> Model
updateModel c model =
  model

update : Msg -> Model -> Model
update msg model =
    case (msg, model) of
        (RequestedStartGame, _ ) ->
            Playing
                {
                      currentPlayer = firstPlayer playersInit,
                      currentCardPlayed = noCard,
                      players = playersInit,
                      drawStack = drawStackInit,
                      discardStack = discardStackInit
                }

        (CardPlayed cardPlayed, Playing game ) ->
            Playing {game |  
                  currentPlayer = firstPlayer <| permutePlayer game.players,
                  currentCardPlayed = cardPlayed,
                  players = permutePlayer <| omitPlayedCard cardPlayed game.players,
                  discardStack = cardPlayed::game.discardStack
             }

        (_ , _ ) ->
          model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

