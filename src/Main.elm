module Main exposing (..)

import Asset exposing (ImageString, imgAvatarBorder1, imgStars, panelAvatars, path, pathFilename, spriteAvatars, spriteCards, src)
import Basics exposing (round)
import Browser
import Debug exposing (log)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, h4, h5, hr, img, input, span, text)
import Html.Attributes exposing (placeholder, style, title)
import Html.Events exposing (onClick, onInput)
import List exposing (append, concat, drop, filter, head, indexedMap, isEmpty, length, map, map2, range, repeat, reverse, sortBy, sum, tail, take)
import Random
import String exposing (fromInt)
import Tuple
import Types exposing (..)
import Update exposing (..)
import Views.Avatar exposing (..)
import Views.GameOver exposing (..)
import Views.Home exposing (homeView)
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
