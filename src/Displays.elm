module Displays exposing (..)

import Asset exposing (..)
import Constants exposing (..)
import Functions exposing (..)
import Html
    exposing
        ( Html
        , a
        , b
        , br
        , button
        , div
        , h1
        , h2
        , h4
        , hr
        , span
        , text
        )
import Html.Attributes
    exposing
        ( style
        , title
        )
import Html.Events
    exposing
        ( onClick
        )
import List
    exposing
        ( head
        , indexedMap
        , length
        , map
        , range
        , sortBy
        , take
        )
import String
    exposing
        ( fromInt
        )
import Types
    exposing
        ( Card
        , CardState(..)
        , Color(..)
        , Game
        , Msg(..)
        , Player
        , Position(..)
        , Reverse(..)
        , StackShadow(..)
        )


displayInfoHand :
    List Card
    -> Card
    -> Html Msg
displayInfoHand hand mainCard =
    text
        (textNbWords
            (nbCardInHand hand)
            "card"
            ++ " - "
            ++ textNbWords (nbPointInHand hand) "point"
        )


displayNextCard :
    Player
    -> Card
    -> Player
    -> Int
    -> Bool
    -> Reverse
    -> Html Msg
displayNextCard player mainCard currentPlayer drawing penality reverse =
    let
        cardNextToBeDisplayed =
            if reverse == ToRight then
                card_NextDown

            else
                card_NextUp
    in
    if currentPlayer == player then
        if
            drawing
                == 0
                || (mainCard.value == 10 && penality == True)
        then
            a [ onClick Pass ] [ displayCard cardNextToBeDisplayed Seen ]

        else
            div [ style "filter" "contrast(50%)" ] [ displayCard cardNextToBeDisplayed Seen ]

    else
        text ""


displayPlayers : List Player -> Card -> Int -> Bool -> Reverse -> Html Msg
displayPlayers players mainCard drawing penality reverse =
    div []
        (map (\p -> displayPlayer p mainCard (headPlayer players) drawing penality reverse)
            (sortBy .id players)
        )


displayPlayer : Player -> Card -> Player -> Int -> Bool -> Reverse -> Html Msg
displayPlayer player mainCard currentPlayer drawing penality reverse =
    div []
        [ hr [] []
        , div
            [ style "padding" "2px"
            , style "background-color" "LIGHTGREEN"
            ]
            [ b [ style "font-size" "20px" ] [ text player.name ]
            , span [] [ text " - " ]
            , span [] [ displayInfoHand player.hand mainCard ]
            , span [] [ text " - " ]
            ]
        , div
            [ style "height" (toPx cZoomAvatar (cHeight + 2 * 10))
            , style "display" "flex"
            , let
                isCurrentPlayer =
                    currentPlayer == player
              in
              case ( isCurrentPlayer, penality ) of
                ( True, True ) ->
                    style "background-color" "RED"

                ( True, False ) ->
                    style "background-color" "GREEN"

                ( False, _ ) ->
                    style "background-color" "WHITE"
            ]
            [ div [ style "flex" "1", style "margin-bottom" "10px", style "margin-left" "50px" ]
                [ displayAvatarCard player.avatar Absolute
                , displayAvatarCard player.avatarStyle Absolute
                ]
            , div
                [ style "flex" "10"
                , style "margin-top" "10px"
                , style "margin-bottom" "10px"
                , style "display" "flex"
                , style "justify-content" "flex-end"
                , style "flex-direction" "column"
                ]
                [ if currentPlayer == player then
                    displayHandCards player.hand mainCard drawing penality

                  else
                    displayCards player.hand NotPlayabled
                ]
            , div [ style "flex" "1", style "margin-top" "10px", style "margin-bottom" "10px" ]
                [ displayNextCard player mainCard currentPlayer drawing penality reverse
                ]
            ]
        ]


displayPlayabled : Card -> Html Msg
displayPlayabled h =
    a [ onClick (CardPlayed h) ] [ displayCard h Playabled ]


displayNotPlayabled : Card -> Html Msg
displayNotPlayabled h =
    displayCard h NotPlayabled


displayRegular : Card -> Card -> Html Msg
displayRegular h mainCard =
    if h.value == mainCard.value || h.color == mainCard.color || h.color == Black then
        displayPlayabled h

    else
        displayNotPlayabled h


displayHandCards : List Card -> Card -> Int -> Bool -> Html Msg
displayHandCards cards mainCard drawing penality =
    div [ style "margin-left" (toPx 1 -cMargin) ]
        (map
            (\h ->
                case ( mainCard.value, mainCard.color, drawing ) of
                    ( 12, _, 0 ) ->
                        if penality then
                            displayNotPlayabled h

                        else
                            displayRegular h mainCard

                    ( 12, _, 1 ) ->
                        displayRegular h mainCard

                    ( 12, _, _ ) ->
                        if h.value == 12 || (h.color == Black && h.value == 13) then
                            --- [+2] ou [+4] jouables
                            displayPlayabled h

                        else
                            displayNotPlayabled h

                    ( 13, _, 0 ) ->
                        if penality then
                            displayNotPlayabled h

                        else
                            displayRegular h mainCard

                    ( 13, _, 1 ) ->
                        displayRegular h mainCard

                    ( 13, _, _ ) ->
                        if h.color == Black && h.value == 13 then
                            --- [+2] ou [+4] jouables
                            displayPlayabled h

                        else
                            displayNotPlayabled h

                    ( 10, _, _ ) ->
                        if penality then
                            displayNotPlayabled h

                        else
                            displayRegular h mainCard

                    ( _, _, _ ) ->
                        displayRegular h mainCard
            )
            cards
        )


displayCards : List Card -> CardState -> Html Msg
displayCards cards cardState =
    div [ style "margin-left" (toPx 1 -cMargin) ] (map (\c -> displayCard c cardState) cards)


displayStack : Int -> StackShadow -> Card -> Html Msg
displayStack index shadow c =
    let
        z =
            cZoom

        marginRight =
            -(90 / 100) * cWidth

        marginLeft =
            -(110 / 100) * cWidth

        i =
            stackThickness - index
    in
    div
        [ style "background-image" ("url(" ++ spriteCards ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        , if shadow == Left then
            style "margin-left" (toPx z marginLeft)

          else
            style "margin-right" (toPx z marginRight)
        , style "z-index" (fromInt i)
        ]
        []


displayCard : Card -> CardState -> Html Msg
displayCard c cardState =
    let
        z =
            cZoom
    in
    div
        [ style "background-image" ("url(" ++ spriteCards ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c cardState))
        , style "background-size" (toPx z cSpriteSize)
        , style "float" "left"
        , style "margin-left" (toPx z cMargin)
        ]
        []


displayActionCard : Card -> Html Msg -> Html Msg
displayActionCard c cartTextDiv =
    let
        z =
            cZoomAction
    in
    div
        [ style "background-image" ("url(" ++ spriteCards ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        , style "z-index" "100"
        ]
        [ cartTextDiv ]


displayAvatarCard : Card -> Position -> Html Msg
displayAvatarCard c position =
    let
        z =
            cZoomAvatar
    in
    div
        [ style "background-image" ("url(" ++ spriteAvatars ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        , case position of
            Absolute ->
                style "position" "absolute"

            Relative ->
                style "position" "relative"

            Static ->
                style "position" "static"
        ]
        []


displayDrawActionCard : Int -> Int -> Int -> Card -> Bool -> Html Msg
displayDrawActionCard drawing nbDrawCards nbDiscardCards masterCard penality =
    let
        cardToBeDisplayed =
            if
                penality
                    && ((masterCard.value == 13 && masterCard.color == Black)
                            || (masterCard.value == 14 && masterCard.color == Black)
                       )
            then
                "cardToBeDisplayed_DrawImpossible"

            else if drawing == 0 then
                if nbDrawCards == 0 then
                    "cardToBeDisplayed_DrawImpossible"

                else
                    "cardToBeDisplayed_Drawed"

            else if nbDrawCards == 0 then
                "cardToBeDisplayed_DrawImpossible"

            else if drawing > nbDrawCards then
                "cardToBeDisplayed_DrawImpossible"

            else if masterCard.value == 10 && masterCard.color /= Black && penality then
                "cardToBeDisplayed_Drawed"

            else
                "cardToBeDisplayed_Draw"
    in
    case cardToBeDisplayed of
        "cardToBeDisplayed_Drawed" ->
            displayActionCard card_Drawed (innerDrawActionCard card_Drawed nbDrawCards drawing)

        "cardToBeDisplayed_DrawImpossible" ->
            displayActionCard card_DrawImpossible (innerDrawActionCard card_DrawImpossible nbDrawCards drawing)

        "cardToBeDisplayed_GameOver" ->
            a [ onClick GameEnded ]
                [ displayActionCard card_Finish (div [] []) ]

        _ ->
            a [ onClick DrawCard, style "z-index" "100" ]
                [ displayActionCard card_Draw (innerDrawActionCard card_Draw nbDrawCards drawing) ]


displayBtnEndGame : Html Msg
displayBtnEndGame =
    button [ onClick GameEnded, style "width" "200px" ] [ text "End the game" ]


displayHeaderRight : Game -> Html Msg
displayHeaderRight game =
    div [ style "margin-right" "50px" ]
        [ a [ onClick GameEnded, title "End the game !", style "float" "right" ]
            [ displayActionCard card_Finish (div [] []) ]
        ]


displayHeaderBoard : Game -> Html Msg
displayHeaderBoard game =
    div [ style "display" "flex" ]
        [ div [ style "flex" "3" ] [ displayHeaderMaster game ]
        , div [ style "flex" "6" ]
            [ displayPlayerName game
            , div [ style "display" "flex" ]
                [ div [ style "flex" "5", style "display" "flex", style "justify-content" "center" ]
                    (map (\( i, c ) -> displayStack i Right c) <|
                        take stackThickness <|
                            indexedMap Tuple.pair game.discardStack
                    )
                , div
                    [ style "flex" "2"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    ]
                    [ let
                        nDrawStack =
                            length game.drawStack

                        nDisgardStack =
                            length game.discardStack
                      in
                      --- je propose de generer si drawing > nDrawStack
                      if game.drawing > nDrawStack then
                        if nDisgardStack > 1 then
                            a [ onClick GenerateDrawStack ]
                                [ displayActionCard card_DrawGenerate (div [] [])
                                ]

                        else
                            let
                                currentPlayer =
                                    headPlayer game.players

                                currentPlayerHand =
                                    currentPlayer.hand
                            in
                            if isHandPlayable (headPlayer game.players) game.mainCard game.drawing game.penality then
                                -- text " Je suis MAL !!! Mais le joueur peut jouer"
                                text ""

                            else
                                -- text " Je suis MAL !!! Le joueur ne peut plus jouer !!!"
                                a [ onClick GameEnded ]
                                    [ displayActionCard card_Finish (div [] [])
                                    ]

                      else
                        text ""
                    ]
                , div
                    [ style "flex" "5"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    ]
                    (displayDrawActionCard game.drawing (length game.drawStack) (length game.discardStack) game.mainCard game.penality
                        :: (map (\( i, c ) -> displayStack i Left card_Back) <|
                                take (stackThickness - 1) <|
                                    indexedMap Tuple.pair game.drawStack
                           )
                    )
                ]
            ]
        , div [ style "flex" "3" ] [ displayHeaderRight game ]
        ]


displayPlayerName : Game -> Html Msg
displayPlayerName game =
    div
        [ style "background-color" "YELLOW"
        ]
        [ text (headPlayer game.players).name
        ]


displayHeaderMaster : Game -> Html Msg
displayHeaderMaster game =
    div []
        [ -- pour FILL
          -- il faut Deck>=2 cartes
          -- il faut Draw vide
          -- il faut Pas déjà Piocher
          if length game.drawStack == 0 && length game.discardStack > 1 && game.drawing > 0 then
            div [] [ text "Click on the DRAW pile to regenerate it." ]

          else
            div [] [ text "" ]

        -- pour End Game
        -- il faut Pas Jouable
        -- il faut Draw vide
        -- il faut Deck <=1 carte
        -- il faut Pas déjà Piocher
        , if
            not (isHandPlayable (headPlayer game.players) game.mainCard game.drawing game.penality)
                && length game.drawStack
                == 0
                && length game.discardStack
                <= 1
                && game.drawing
                > 0
          then
            div [] [ displayBtnEndGame ]

          else
            div [] [ text "" ]
        , div [ style "background-color" "LIGHTYELLOW" ]
            [ if length game.drawStack == 0 then
                h4 []
                    [ text "The DRAW pile is depleted !"
                    , if length game.discardStack <= 1 && game.drawing > 0 then
                        div []
                            [ text "there's not any card left."
                            ]

                      else
                        text ""
                    ]

              else
                div [] [ text "" ]
            ]
        , if
            game.penality
                && ((game.mainCard.value == 13 && game.mainCard.color == Black)
                        || (game.mainCard.value == 14 && game.mainCard.color == Black)
                   )
          then
            div []
                [ text "Choose your color"
                , div [ style "display" "flex", style "justify-content" "center" ]
                    (map
                        (\i ->
                            a [ onClick (SetWildCardColor (convertIntToColor i)) ]
                                [ displayCard { id = 0, value = 14, color = convertIntToColor i } Seen
                                ]
                        )
                        (range 1 4)
                    )
                ]

          else
            div [] [ text "" ]
        , if game.penality && game.mainCard.value == 10 && game.mainCard.color /= Black then
            div [ style "background-color" "LIGHTYELLOW" ]
                [ text "Your turn's skipped ! Any action isn't possible. Click on NEXT." ]

          else
            div [] [ text "" ]
        ]


displayWinner : List Player -> Html Msg
displayWinner players =
    div []
        [ case
            head
                (sortBy .pts (map (\p -> { name = p.name, pts = nbPointInHand p.hand }) players))
          of
            Just winner ->
                if winner.pts == 0 then
                    text winner.name

                else
                    text (winner.name ++ " with " ++ textNbWords winner.pts "point")

            Nothing ->
                text "Nobody wins"
        ]


innerDrawActionCard : Card -> Int -> Int -> Html Msg
innerDrawActionCard cardToBeDisplayed nbDrawCards drawing =
    let
        displayFlexTop =
            if drawing > 0 then
                [ text "Draw", br [] [], text ("+" ++ fromInt drawing) ]

            else
                [ text "  ", br [] [], text " " ]
    in
    displayActionCard cardToBeDisplayed
        (div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "height" (toPx cZoomAction cHeight)
            , style "color" "YELLOW"
            ]
            [ div
                [ style "flex" "50%"
                , style "margin-top" "20px"
                , style "font-size" "20px"
                ]
                displayFlexTop
            , div
                [ style "flex" "50%"
                , style "font-size" "13px"
                , style "margin-bottom" "-15px"
                ]
                [ text (fromInt nbDrawCards), br [] [], text (textWord nbDrawCards "card"), br [] [], text "left" ]
            ]
        )
