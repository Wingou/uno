module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, h4, h5, hr, span, text, img, input)
import Html.Attributes exposing (style, title, placeholder)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, head, isEmpty, length, map, reverse, sortBy, sum, tail, repeat, range, map2, take, drop, concat, append, indexedMap)
import String exposing (fromInt)
import Basics exposing (round)
import Random
import Asset exposing (src, ImageString, path, pathFilename, imgStars, spriteCards, spriteAvatars, panelAvatars, imgAvatarBorder1)
import Tuple
import Debug exposing (log)

import Views.Home exposing (homeView)
import Views.GameOver exposing (..)
import Types exposing (..)
import Update exposing (..)
import Views.Avatar exposing(..)
import Views.Playing exposing(..)

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
    (NotStarted, Cmd.none)


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