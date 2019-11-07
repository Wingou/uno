module Views.GameOver exposing (..)

import Types exposing(Player, Msg, Reverse (..))
import Html exposing (Html)
import Html exposing (div, h1, h2, hr, text)
import Functions exposing (..)
import Displays exposing (displayPlayers, displayWinner)
import Constants exposing (..)


gameOver : List Player -> Html Msg
gameOver players =
    div []
        [ h1 [] [ text "GAME OVER" ]
        , hr [] []
        , h2 [] [ text "The winner is " ]
        , h2 [] [ displayWinner players ]
        , hr [] []
        , displayPlayers players card_Back 0 False ToRight
        ]