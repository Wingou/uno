module Views.Avatar exposing (..)

import Constants exposing (..)
import Displays exposing (..)
import Functions exposing (..)
import Html exposing (Html, a, b, br, button, div, h3, h4, hr, input, span, text)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (all, map, range)
import String exposing (fromInt, length)
import Types exposing (..)


displayChooseName : AvatarModel -> Html Msg
displayChooseName model =
    div [ style "margin" "20px" ]
        [ hr [] []
        , div [ style "background-color" "LIGHTPINK" ] [ text "Choose your name" ]
        , br [] []
        , div []
            [ if model.avatarId > 0 then
                input [ onInput SetPlayerName, value model.inputName ] []

              else
                text "Please, click on [Setting] first !"
            ]
        ]


displayChooseAvatar : AvatarModel -> Html Msg
displayChooseAvatar model =
    div [ style "margin" "20px" ]
        [ hr [] []
        , div [ style "background-color" "YELLOW" ] [ text "Choose your avatar" ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
            ]
            (map
                (\v ->
                    div
                        [ if model.inputAvatar == v then
                            style "cursor" "not-allowed"

                          else
                            style "cursor" "pointer"
                        , onClick (SetAvatar v)
                        ]
                        [ displayAvatarCard (getCard v Red) Absolute
                        , if model.inputAvatar == v then
                            displayAvatarCard (getCard 14 Black) Relative

                          else
                            displayAvatarCard (getCard 13 Black) Relative
                        ]
                )
                (range 1 nbAvatars)
            )
        ]


displayChooseStyle : AvatarModel -> Html Msg
displayChooseStyle model =
    div [ style "margin" "20px" ]
        [ hr [] []
        , div [ style "background-color" "LIGHTBLUE" ] [ text "Choose your card style" ]
        , div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
            ]
            (map
                (\v ->
                    div [ onClick (SetAvatarStyle v) ]
                        [ displayAvatarCard (getCard v Black) Absolute
                        , if model.inputAvatarStyle == v then
                            displayAvatarCard (getCard 14 Black) Relative

                          else
                            displayAvatarCard (getCard 13 Black) Relative
                        ]
                )
                (range 1 nbAvatarsStyles)
            )
        ]


displayGenerator : AvatarModel -> Html Msg
displayGenerator model =
    div [ style "margin" "20px" ]
        [ let
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
                , button [ style "cursor" "pointer", onClick RandomizeAvatar ] [ text "Auto-Random Avatars Generation" ]
                , div [] [ text "OR" ]
                , div [] [ text "Click on the Setting button of a Player" ]
                ]

          else
            div []
                [ div [ style "background-color" bgColor ] [ text "Setting..." ]
                , br [] []
                , text "Choose a name, choose an Avatar and a card style."
                , br [] []
                , text "To submit your choice click on [I'm ready] !"
                ]
        ]


displaySettingPlayers : AvatarModel -> Html Msg
displaySettingPlayers model =
    let
        playerId =
            model.avatarId

        isAvatarChosen =
            model.inputAvatar > 0 && model.inputAvatarStyle > 0 && length model.inputName > 0
    in
    div []
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
                            b [ style "color" "DARKGREEN" ]
                                [ text (p.name ++ " Ready !")
                                ]

                          else
                            text p.name
                        ]
                    , button [ onClick (SetAvataring p) ]
                        [ text "Setting"
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


setAvatarView : AvatarModel -> Html Msg
setAvatarView model =
    div []
        [ div []
            [ div [ style "display" "flex" ]
                [ --------- GAUCHE - SETTING PLAYERS
                  div [ style "flex" "2", style "margin" "10px" ]
                    [ b [] [ text "PLAYERS" ]
                    , displaySettingPlayers model
                    ]

                --------- DROITE - CHOICE AVATARS OPTIONS
                , div [ style "flex" "10", style "margin-top" "10px" ]
                    [ displayGenerator model
                    , if model.avatarId > 0 then
                        displayChooseName model

                      else
                        text ""
                    , if model.avatarId > 0 then
                        displayChooseAvatar model

                      else
                        text ""
                    , if model.avatarId > 0 then
                        displayChooseStyle model

                      else
                        text ""
                    ]
                ]
            ]
        ]
