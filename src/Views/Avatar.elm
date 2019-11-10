module Views.Avatar exposing (..)

import Constants exposing (..)
import Displays exposing (..)
import Functions exposing (..)
import Html exposing (Html, b, br, button, div, h3, hr, input, span, text)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (all, map, range)
import String exposing (fromInt, length)
import Types exposing (..)


setAvatarView : AvatarModel -> Html Msg
setAvatarView model =
    div []
        [ let
            playerId =
                model.avatarId

            isAvatarChosen =
                model.inputAvatar > 0 && model.inputAvatarStyle > 0 && length model.inputName > 0
          in
          div []
            [ div [ style "display" "flex" ]
                [ --------- GAUCHE
                  div [ style "flex" "6", style "margin-top" "10px" ]
                    [ -- --- NAME ---------------------
                      div [ style "background-color" "LIGHTPINK" ] [ text "Choose your name" ]
                    , div []
                        [ input [ onInput SetPlayerName, value model.inputName ] []
                        ]
                    , hr [] []
                    , -- --- AVATAR ---------------------
                      div []
                        [ div [ style "background-color" "YELLOW" ] [ text "Choose your avatar" ]
                        , div
                            [ style "display" "flex"
                            , style "justify-content" "center"
                            , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
                            ]
                            (map
                                (\v ->
                                    div [ onClick (SetAvatar v) ]
                                        [ displayAvatarCard (getCard v Red) Static ]
                                )
                                (range 1 nbAvatars)
                            )
                        ]
                    , hr [] []

                    -- --- STYLE ---------------------
                    , div []
                        [ div [ style "background-color" "LIGHTBLUE" ] [ text "Choose your card style" ]
                        , div
                            [ style "display" "flex"
                            , style "justify-content" "center"
                            , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
                            ]
                            (map
                                (\v ->
                                    div [ onClick (SetAvatarStyle v) ]
                                        [ displayAvatarCard (getCard v Black) Static ]
                                )
                                (range 1 nbAvatarsStyles)
                            )
                        ]
                    , hr [] []
                    , let
                        bgColor =
                            "LIGHTGREEN"
                      in
                      if all (\p -> p.isNeoPlayerChecked == True) model.neoPlayers then
                        div []
                            [ div [ style "background-color" bgColor ] [ text "ALL THE PLAYERS ARE READY TO START THE GAME !" ]
                            , br [] []
                            , button [ onClick RequestedStartGame ] [ h3 [] [ text "   Let's play" ] ]
                            ]

                      else if model.avatarId == 0 then
                        div []
                            [ div [ style "background-color" bgColor ] [ text "AVATARS GENERATOR" ]
                            , br [] []
                            , button [ onClick RandomizeAvatar ] [ h3 [] [ text "Random Avatars Generation" ] ]
                            ]

                      else
                        div []
                            [ div [ style "background-color" bgColor ] [ text "Choose your avatar" ]
                            , br [] []
                            , text "Enter your name, choose your Avatar and a card style."
                            , br [] []
                            , text "Then to submit your choice, click on READY !"
                            ]
                    ]

                --------- DROITE
                , div [ style "flex" "6", style "margin-top" "10px" ]
                    [ ------- RESULTS ---------------------------------
                      b [] [ text "PLAYERS" ]
                    , div []
                        (map
                            (\p ->
                                div []
                                    [ -- --- Name ---------------------
                                      hr [] []
                                    , div
                                        [ style "margin-top" "10px" ]
                                        [ if playerId == p.id then
                                            text model.inputName

                                          else if p.isNeoPlayerChecked then
                                            b [ style "color" "RED" ]
                                                [ text ("Ready " ++ p.name ++ " !")
                                                ]

                                          else
                                            text p.name
                                        ]
                                    , button [ onClick (SetAvataring p) ]
                                        [ text "Choose avatar"
                                        ]
                                    , span [] [ text " " ]
                                    , let
                                        readyButtonState =
                                            if playerId == p.id && isAvatarChosen then
                                                "enabled"

                                            else
                                                "disabled"
                                      in
                                      button
                                        [ onClick (ValiderAvatar playerId)
                                        , attribute readyButtonState ""
                                        ]
                                        [ text "I'm ready"
                                        ]
                                    , let
                                        avatarToDisplay =
                                            if playerId == p.id && not p.isNeoPlayerChecked then
                                                getCard model.inputAvatar Red

                                            else
                                                p.avatar

                                        avatarStyleToDisplay =
                                            if playerId == p.id && not p.isNeoPlayerChecked then
                                                getCard model.inputAvatarStyle Black

                                            else
                                                p.avatarStyle
                                      in
                                      div
                                        [ style "display" "flex"
                                        , style "justify-content" "center"
                                        , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
                                        ]
                                        [ displayAvatarCard avatarToDisplay Absolute
                                        , displayAvatarCard avatarStyleToDisplay Absolute
                                        ]
                                    ]
                            )
                            model.neoPlayers
                        )
                    ]
                ]
            ]
        ]
