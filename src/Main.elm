module Main exposing (..)

import Browser
import Html exposing (Html)
import Types exposing (..)
import Update exposing (..)
import Views.Avatar exposing (..)
import Views.GameOver exposing (..)
import Views.Home exposing (..)
import Views.Playing exposing (..)


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( NotStarted, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            homeView

        SettingAvatar avatars ->
            setAvatarView avatars

        Playing game ->
            playingView game

        GameOver players ->
            gameOver players


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
