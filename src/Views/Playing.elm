module Views.Playing exposing (..)

import Displays exposing (..)
import Html exposing (Html, div, h4, hr, text)
import Html.Attributes exposing (style)
import Types exposing (..)


playingView : Game -> Html Msg
playingView game =
    div [ style "background-color" "AZURE" ]
        [ displayHeaderBoard game
        , displayPlayers game.players game.mainCard game.drawing game.penality game.reverse
        , hr [] []
        , div []
            [ displayBtnEndGame
            , h4 [] [ text "UNO - Workshop ELM - October 2019" ]
            ]
        ]
