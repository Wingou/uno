
module Views.Home exposing (homeView)

import Html exposing (..)
import Types exposing (Msg (..) )
import Html.Attributes exposing (style)
import Asset exposing (..)
import Update exposing (..)
import Html.Events exposing (onClick, onInput)


homeView : Html Msg
homeView =
    div []
        [ br [] []
        , h1 [] [ text "UNO - ELM" ]
        , button [ onClick RequestSetAvatar ]
            [ h3 []
                [ text "  LET'S START...  "
                ]
            ]
            , br [][]
            , br [][]
        , 
        div[style "background-image" ("url(" ++ (pathFilename imgStars) ++ ")")
        , style "width" "100%"
        , style "height" "550px"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center"
        , style "background-attachment" "fixed"
        , style "background-position" "center" 
        ][]
    ]