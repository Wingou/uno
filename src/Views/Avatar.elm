module Views.Avatar exposing(..)

import Types exposing (..)
import Html exposing (Html, div, text, button, input)
import Html.Attributes exposing (style, placeholder)
import Functions exposing (..)
import Constants exposing (..)
import List exposing (map, range)
import Html.Events exposing (onClick, onInput)
import Displays exposing (..)

setAvatarView : PlayerModel -> Html Msg
setAvatarView model =
    div [][
            let
                withoutAvatarPlayer = avatarToBeSet model.players
                playerId = withoutAvatarPlayer.id
            in
            -- if withoutAvatarPlayer == noPlayer then
            --     div[][text "Plus de joueurs disponible"]
            -- else
                div[][
                      div [style "display" "flex"][
                             --------- GAUCHE
                             div[style "flex" "6"][
                                    -- --- AVATAR --------------------- 
                                     div [][
                                        div [ style "background-color" "YELLOW"][ text "Choose your avatar"]
                                        , div [
                                              style "display" "flex",
                                                    style "justify-content" "center", 
                                                    style "height" (toPx cZoomAvatar (cHeight-2*cMargin))
                                        ]
                                                 (map (\v -> 
                                                         div [ onClick (SetAvatar v) ]
                                                             [displayAvatarCard (getCard v Red) Static]
                                                      )
                                                      (range 1 5))
                                                ]
                                    -- --- STYLE --------------------- 
                                    , div [][
                                        div [ style "background-color" "YELLOW"][ text "Choose your style"]
                                        , div [
                                              style "display" "flex",
                                                    style "justify-content" "center", 
                                                    style "height" (toPx cZoomAvatar (cHeight-2*cMargin))
                                        ]
                                                (map (\v -> 
                                                        div [ onClick (SetAvatarStyle v) ]
                                                            [displayAvatarCard (getCard v Black) Static]
                                                    )
                                                    (range 1 5))
                                            ]
                                            

                                    -- --- Example AVATAR -------------
                                    , div [][
                                            div [ style "background-color" "YELLOW"][ text "To submit your choice, click on your Avatar card below " ]
                                            , div [
                                                onClick (ValiderAvatar playerId),
                                                  style "display" "flex",
                                                    style "justify-content" "center", 
                                                    style "height" (toPx cZoomAvatar (cHeight-2*cMargin))]
                                            [
                                                displayAvatarCard (getCard model.inputAvatar Red) Absolute,
                                                displayAvatarCard (getCard model.inputAvatarStyle Black) Absolute
                                            ]
                                        ]

                                     -- --- LET GO ! -------------
                                    , div[][
                                         div [ style "background-color" "YELLOW"][ text "START" ]
                                         , button [ onClick RequestedStartGame ] [ text "Let's start"]
                                    ]
                             ]
                             --------- DROITE
                            ,div[style "flex" "6"][

                                ------- RESULTS ---------------------------------
                                div [][ div [ style "background-color" "YELLOW"][ text "AVATARS" ]]
                                , div[]
                                    (map (\p ->
                                         div[][   
                                                    -- --- Name --------------------- 
                                                    div []
                                                        [
                                                            input [ onInput SetPlayerName, placeholder "Your name here" ][]
                                                       ],

                                                div [style "margin-top" "10px"][
                                                    text p.name
                                                ],
                                                div[
                                                    style "display" "flex",
                                                    style "justify-content" "center", 
                                                    style "height" (toPx cZoomAvatar (cHeight-2*cMargin))
                                                    ]
                                                    [
                                                        displayAvatarCard p.avatar Absolute,
                                                        displayAvatarCard p.avatarStyle Absolute
                                                    ]
                                            ]
                                            ) model.players
                                    )
                                -- div [
                                --     style "display" "flex"
                                --     --, style "flex-direction" "column"
                                -- ]
                                -- div[]
                                --     (map (\p ->
                                --             div[style "display" "flex", style "border" "solid"][
                                --                 div[][ text "PLAYERS 1"],
                                --                 div[][
                                --                     displayAvatarCard p.avatar Absolute,
                                --                     displayAvatarCard p.avatarStyle Absolute
                                --                 ]
                                --             ]
                                --         ) model.players
                         --)

                            ]

                    ]



                    -- --- Name --------------------- 
                    -- , div []
                    --     [
                    --         h3 [ style "background-color" "YELLOW"][ text "Enter your name"]
                    --         , input [ onInput SetPlayerName, placeholder "Your name here" ][]
                    -- ]
                    -- --- AVATAR --------------------- 
                    -- , div [][
                    --     h3 [ style "background-color" "YELLOW"][ text "Choose your avatar"]
                    --     , div [style "display" "flex", style "justify-content" "center"]
                    --             (map (\v -> 
                    --                     div [ onClick (SetAvatar v) ]
                    --                         [displayAvatarCard (getCard v Red) Static]
                    --                  )
                    --                  (range 1 3))
                    --            ]
                    -- --- STYLE --------------------- 
                    -- , div [][
                    --     h3 [ style "background-color" "YELLOW"][ text "Choose your style"]
                    --     , div [style "display" "flex", style "justify-content" "center"]
                    --             (map (\v -> 
                    --                     div [ onClick (SetAvatarStyle v) ]
                    --                         [displayAvatarCard (getCard v Black) Static]
                    --                  )
                    --                  (range 1 2))
                    --            ]
                    -- , div [][
                    --     h3 [ style "background-color" "YELLOW"][ text "To submit your choice, click on your Avatar card below " ]
                    --     , div [style "display" "flex", style "justify-content" "center",  onClick (ValiderAvatar playerId)]
                    --           [
                    --             displayAvatarCard (getCard model.inputAvatar Red) Absolute,
                    --             displayAvatarCard (getCard model.inputAvatarStyle Black) Absolute
                    --           ]
                    --         ]
                    -- , hr[][]
                    -- , div []
                    --     [h3 [ style "background-color" "YELLOW"][ text "AVATAR" ]]
                    -- , div [style "display" "flex", style "justify-content" "center"]
                    --     (map (\p ->
                    --             div[][
                    --                 displayAvatarCard p.avatar Absolute,
                    --                 displayAvatarCard p.avatarStyle Absolute
                    --             ]
                    --         ) model.players
                    --     )

                    -- , button [ onClick RequestedStartGame ] [ text "Let's start"]
                ]
    ]
