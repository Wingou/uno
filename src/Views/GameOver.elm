module Views.GameOver exposing (..)

import Constants exposing (..)
import Displays exposing (displayPlayers, displayWinner)
import Functions exposing (..)
import Html exposing (Html, div, h1, h2, h3, hr, text)
import Types exposing (Msg, Player, Reverse(..))


gameOver : List Player -> Html Msg
gameOver players =
    div []
        [ h2 [] [ text "GAME OVER" ]
        , h3 [] [ text "The winner is " ]
        , h2 [] [ displayWinner players ]
        , displayPlayers players card_Back 99 False ToStay
        ]
