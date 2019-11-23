module Views.Home exposing (homeView)

import Asset exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Svg exposing (circle)
import Svg.Attributes exposing (cx, cy, r)
import Types exposing (Msg(..))
import Update exposing (..)


homeView : Html Msg
homeView =
    div [ style "margin-top" "10px" ]
        [ button [ onClick RequestSetAvatar ]
            [ div [ style "font-size" "20px" ]
                [ text "  UNO - ELM   " ]
            ]
        , br [] []
        , br [] []
        , div
            [ style "background-image" ("url(" ++ pathFilename imgStars ++ ")")
            , style "width" "100%"
            , style "height" "650px"
            , style "background-repeat" "no-repeat"
            , style "background-position" "center"
            , style "background-attachment" "fixed"
            , style "background-position" "center"
            ]
            []
        ]
