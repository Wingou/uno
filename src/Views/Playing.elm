
module Views.Playing exposing(..)

import Types exposing (..)
import Html exposing (Html, div, hr, h4, text)
import Html.Attributes exposing (style)
import Displays exposing (..)



playingView : Game -> Html Msg
playingView game =
    div [ style "background-color" "AZURE" ]
        [ 
         displayHeaderBoard game
        , displayPlayers game.players game.mainCard game.drawing game.penality game.reverse
        , hr [] []
        , div []
            [ displayBtnEndGame
            , h4 [] [ text "UNO - Workshop ELM - October 2019" ]
            ]
        ]

