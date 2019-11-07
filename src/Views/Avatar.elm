module Views.Avatar exposing (..)

import Constants exposing (..)
import Displays exposing (..)
import Functions exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onClick, onInput)
import List exposing (map, range)
import Types exposing (..)


setAvatarView : PlayerModel -> Html Msg
setAvatarView model =
    div []
        [ let
            withoutAvatarPlayer =
                avatarToBeSet model.players

            playerId =
                withoutAvatarPlayer.id
          in
          div []
            [ div [ style "display" "flex" ]
                [ --------- GAUCHE
                  div [ style "flex" "6" ]
                    [ -- --- AVATAR ---------------------
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
                                (range 1 5)
                            )
                        ]

                    -- --- STYLE ---------------------
                    , div []
                        [ div [ style "background-color" "YELLOW" ] [ text "Choose your style" ]
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
                                (range 1 5)
                            )
                        ]

                    -- --- Example AVATAR -------------
                    , div []
                        [ div [ style "background-color" "YELLOW" ] [ text "To submit your choice, click on your Avatar card below " ]
                        , div
                            [ onClick (ValiderAvatar playerId)
                            , style "display" "flex"
                            , style "justify-content" "center"
                            , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
                            ]
                            [ displayAvatarCard (getCard model.inputAvatar Red) Absolute
                            , displayAvatarCard (getCard model.inputAvatarStyle Black) Absolute
                            ]
                        ]

                    -- --- LET GO ! -------------
                    , div []
                        [ div [ style "background-color" "YELLOW" ] [ text "START" ]
                        , button [ onClick RequestedStartGame ] [ text "Let's start" ]
                        ]
                    ]

                --------- DROITE
                , div [ style "flex" "6" ]
                    [ ------- RESULTS ---------------------------------
                      div [] [ div [ style "background-color" "YELLOW" ] [ text "AVATARS" ] ]
                    , div []
                        (map
                            (\p ->
                                div []
                                    [ -- --- Name ---------------------
                                      div []
                                        [ input [ onInput SetPlayerName, placeholder "Your name here" ] []
                                        ]
                                    , div [ style "margin-top" "10px" ]
                                        [ text p.name
                                        ]
                                    , div
                                        [ style "display" "flex"
                                        , style "justify-content" "center"
                                        , style "height" (toPx cZoomAvatar (cHeight - 2 * cMargin))
                                        ]
                                        [ displayAvatarCard p.avatar Absolute
                                        , displayAvatarCard p.avatarStyle Absolute
                                        ]
                                    ]
                            )
                            model.players
                        )
                    ]
                ]
            ]
        ]
