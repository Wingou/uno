module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, span, br, h1, h2, h3, h4, hr, a, b)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, style)
import String exposing (fromInt)
import List exposing (map, head, tail, reverse, filter, length, sum, sortBy)
import Debug exposing (log)

type Stack = Hand 
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
    { currentPlayer : Player
    , currentCardPlayed : Card
    , players : List Player
    , drawStack : List Card
    , discardStack : List Card
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

cardColor : Color -> String
cardColor c =
  case c of 
    Yellow -> "Brown"
    _ -> convertColorToString c

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
    div[][
        br[][],
        h1[][ text "UNO"],
        button [ onClick RequestedStartGame ] [  text "Press to start..." ]
    ]
    



playingView : Game -> Html Msg
playingView game =
    div []
        [ h3 [] [ text "PLAYER : ", text (firstPlayer game.players).name ]
        , hr [] [ displayPlayers game.players (firstCard game.discardStack)]
        , 
        if (length game.drawStack==0) then
        h3 [] [ text " The deck is empty :(  ", 
              br[][],
              br[][], 
                if (length game.discardStack==1) then
                text "Not enough card to refill the deck !"
                else
                button[ onClick RefillDrawStack ][ text "Refill the deck !" ],
              br[][], button[ onClick GameEnded ][ text "Press here to end the game." ] 
              ]
        else
        h3 [] [ button[ onClick DrawCard ][ text "   >> DRAW A CARD <<   ", br[][], text (fromInt (length game.drawStack) ++ " cards left" ) ]
        -- , a[ onClick GameEnded ][ text "GAME OVER" ]
        ]
        -- , hr [] [displayCards game.drawStack ]
        , hr [] [ ]
        , h3 [] [ text "GAME BOARD", 
              div[style "background-color" "PINK"][
                displayCard <| firstCard game.discardStack
              ]
              ]
        , hr [] [displayCards game.discardStack ]
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

displayInfoHand : List Card -> Card -> Html Msg
displayInfoHand hand masterCard =
      if (masterCard==noCard) then
        text ((fromInt <| nbPointInHand hand)++" points" )
      else
        if nbCardInHand hand==0 then
        span [ onClick GameEnded  ][ 
                  button [][ text "Click here !" ]
            ] 
        else
        text ((fromInt <| nbCardInHand hand) ++" cards" )
    

    
displayPlayers : List Player -> Card -> Html Msg
displayPlayers players masterCard =
   div [] (map (\p -> displayPlayer p masterCard (firstPlayer players) ) players)

displayPlayer : Player -> Card -> Player -> Html Msg
displayPlayer player masterCard currentPlayer =
    div [] [ br[][],
        b[][text player.name, text " : ", displayInfoHand player.hand masterCard ], 
        if currentPlayer==player then
        displayHandCards player.hand masterCard
        else
        displayCards player.hand,
        br[][]
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


displayHandCards : List Card -> Card -> Html Msg
displayHandCards cards masterCard =
    div []  (map (\c -> 
                if c.value==masterCard.value || c.color==masterCard.color
                then
                a[ onClick (CardPlayed c), style "background-color" "PINK" ]
                [ displayCard c]
                else
                displayCard c
        ) cards)


displayCards : List Card ->  Html Msg
displayCards cards  =
    div []  (map (\c -> displayCard c) cards)


displayCard: Card -> Html Msg
displayCard c =
    span[][
      span[][ text " <" ],
      span[]
            [ text (fromInt c.value) ],
      span[]
            [ text " " ],
      span[ style "color" (cardColor c.color)]
            [ text (convertColorToString c.color) ], 
      span[][ text "> "]
    ]
   

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
  div [] [
       case 
          head (
          sortBy .pts (map (\p-> { name=p.name, pts=nbPointInHand p.hand }) players)
          )
        of
        Just winner ->
            if winner.pts ==0 then
              text (winner.name)
            else
              text (winner.name ++ " with " ++ fromInt winner.pts ++ " points")
            
        Nothing ->
            text "Personne"
    ]

gameOver : List Player -> Html Msg
gameOver players =
    div []
        [ 
          h1 [] [ text "GAME OVER" ], 
          hr [][],
          h2 [] [ text "The winner is "],
          h2 [] [displayWinner players ],
          hr [][],
          displayPlayers players noCard
        ]


permutePlayer : List Player -> List Player
permutePlayer listPlayers =
    case (tail listPlayers) of
        Just tailPlayers ->
             reverse ((firstPlayer listPlayers) ::  reverse tailPlayers)
             
        Nothing ->
            []

omitCard: Card -> List Card -> List Card 
omitCard cardToOmit listCards =
    filter (\c -> c/=cardToOmit ) listCards



omitPlayedCard : Card -> List Player -> List Player
omitPlayedCard cardToOmit allPlayers =
  map (\p -> { p | hand=omitCard cardToOmit p.hand} ) allPlayers 

drawCardToPlayer : List Player -> List Card -> List Player
drawCardToPlayer players cards =
    map (\p -> { p| hand = 
                    if (firstPlayer players).id==p.id then
                      reverse ((firstCard cards) :: reverse p.hand )
                    else
                    p.hand
                }
        ) players


tailCard : List Card -> List Card 
tailCard cards =
    case tail cards of
      Just c ->
        c
      Nothing ->
        []

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
    case (msg, model) of
        (RequestedStartGame, _ ) ->
            Playing
                {
                      currentPlayer = firstPlayer playersInit,
                      currentCardPlayed = noCard,
                      players = playersInit,
                      drawStack = omitCard (firstCard drawStackInit) drawStackInit,
                      discardStack = firstCard drawStackInit::[]
                }

        (CardPlayed cardPlayed, Playing game ) ->
            Playing {game |  
                  currentPlayer = firstPlayer <| permutePlayer game.players,
                  currentCardPlayed = cardPlayed,
                  players = permutePlayer <| omitPlayedCard cardPlayed game.players,
                  discardStack = cardPlayed::game.discardStack
             }
        (DrawCard, Playing game) ->
            Playing { game |
                  players = permutePlayer <| drawCardToPlayer game.players game.drawStack,
                  drawStack = omitCard (firstCard game.drawStack) game.drawStack
            }

        (RefillDrawStack, Playing game) ->
            Playing { game |
                  drawStack = tailCard game.discardStack,
                  discardStack = firstCard game.discardStack::[]
            }

        (DoNothing, _ ) ->
          model

        (GameEnded, Playing game ) ->
            GameOver game.players

        (_ , _ ) ->
          model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

