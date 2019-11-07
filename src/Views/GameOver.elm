module Views.GameOver exposing (..)

import Constants exposing (..)
import Displays exposing (displayPlayers, displayWinner)
import Functions exposing (..)
import Html exposing (Html, div, h1, h2, hr, text)
import Types exposing (Msg, Player, Reverse(..))


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
