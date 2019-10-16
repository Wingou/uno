module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing(map)
import String exposing (fromInt)
--import Random.List exposing(shuffle)


type alias Model =
    { count : Int }

type alias Card =
    { value : Int
    , color : Color
    }

type Color
    = Red
    | Green
    | Blue
    | Yellow
    | White
    
    

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


initialModel : Model
initialModel =
    { count = 0 }

drawStackInit : List Card
drawStackInit =
    [ { value = 1
      , color = Red
      }
    , { value = 2
      , color = Red
      }
    , { value = 3
      , color = Red
      }
    , { value = 4
      , color = Red
      }
    , { value = 5
      , color = Red
      }
    , { value = 6
      , color = Red
      }
    , { value = 7
      , color = Red
      }
    , { value = 8
      , color = Red
      }
    , { value = 9
      , color = Red
      }
    , { value = 1
      , color = Blue
      }
    , { value = 2
      , color = Blue
      }
    , { value = 3
      , color = Blue
      }
    , { value = 4
      , color = Blue
      }
    , { value = 5
      , color = Blue
      }
    , { value = 6
      , color = Blue
      }
    , { value = 7
      , color = Blue
      }
    , { value = 8
      , color = Blue
      }
    , { value = 9
      , color = Blue
      }
    , { value = 1
      , color = Yellow
      }
    , { value = 2
      , color = Yellow
      }
    , { value = 3
      , color = Yellow
      }
    , { value = 4
      , color = Yellow
      }
    , { value = 5
      , color = Yellow
      }
    , { value = 6
      , color = Yellow
      }
    , { value = 7
      , color = Yellow
      }
    , { value = 8
      , color = Yellow
      }
    , { value = 9
      , color = Yellow
      }
    , { value = 1
      , color = Green
      }
    , { value = 2
      , color = Green
      }
     , { value = 3
      , color = Green
      }
    , { value = 4
      , color = Green
      }
    , { value = 5
      , color = Green
      }
    , { value = 6
      , color = Green
      }
    , { value = 7
      , color = Green
      }
    , { value = 8
      , color = Green
      }
    , { value = 9
      , color = Green
      }
    ]


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
         (map
             (\d -> div[][ text ("<"++fromInt(d.value)++" "++convertColorToString(d.color)++"> ") ] )
            drawStackInit
         )
    



main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
