module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, h4, h5, hr, span, text, img)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import List exposing (filter, head, isEmpty, length, map, reverse, sortBy, sum, tail, repeat, range, map2, take, drop, concat, append, indexedMap)
import String exposing (fromInt)
import Basics exposing (round)
import Random
import Asset exposing (src, unoSprite, Image, path, pathFilename)
import Tuple
import Debug exposing (log)

type CardState
    = Playabled
    | NotPlayabled
    | Seen


type Stack
    = Hand
    | Draw
    | Discard


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Black

type StackShadow = Right | Left

type alias Card =
    { id : Int
    , value : Int
    , color : Color
    }


type alias Player =
    { id : Int
    , name : String
    , hand : List Card
    }

type Reverse = ToRight | ToLeft | ToStay

type alias Game =
    { 
      originStack : List Card
    , mainCard : Card
    , reverse : Reverse
    , players : List Player
    , drawStack : List Card
    , discardStack : List Card
    , drawing : Int
    , penality : Bool
    }
  

type Model
    = NotStarted
    | Playing Game
    | GameOver (List Player)


type Msg
    = RequestedStartGame
    | CardPlayed Card
    | DrawCard
    | DoNothing
    | GameEnded
    | RefillDrawStack
    | Pass
    | DemandeNewListCards
    | DistributeDrawStack (List Int)
    | SetWildCardColor Color

--- Build --> 2 x 4 couleurs x (de 1 à 12 <=> 1, 2, ..., 9 + 3 Spé )
buildRegular : List Card
buildRegular =
    concat ( map (\_->
        concat (
            map (\c->
                map (\v -> { id=0, value=v, color=convertIntToColor(c)} ) (range 1 12)
            ) (range 1 4)
        )
    ) (range 1 2)
    )

--- Build --> 4 couleurs de la carte 0
buildZero : List Card
buildZero =
        map (\c -> { id=0, value=0, color=convertIntToColor(c)} ) (range 1 4)

--- Build --> 4 x les 2 jokers
buildBlack : List Card
buildBlack = 
    concat (
        map (\_ -> 
            map (\v -> { id=0, value=v, color=Black}) (range 13 14)
        ) (range 1 4)
    )

drawStackInit : List Card
drawStackInit = 
    buildZero ++ 
    buildRegular ++
    buildBlack
    

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
    (NotStarted, Cmd.none)


discardStackInit : List Card
discardStackInit = []

--------------------------
------------- CardValues

------- Spe Cards
--x_valueBlock : Int
--x_valueBlock =10

--x_valueReverse : Int
--x_valueReverse=11

--x_valuePlus2 : Int
--x_valuePlus2 = 12

------- Black Cards
--x_valuePlus4 :  Int
--x_valuePlus4 =13

--x_valueJoker : Int
--x_valueJoker=14

------------- CardValues
--------------------------

toPx : Float -> Float -> String
toPx zoom v =
    fromInt (round (v*zoom)) ++ "px"

cZoomAction : Float
cZoomAction=1.0

cZoom : Float
cZoom = 1.0

cZoomAvatar : Float
cZoomAvatar = 1.2


cPlayableUp : Float
cPlayableUp =13
    --round(13*cZoom)

cSpriteSize : Float
cSpriteSize =1400
    --round (1216*cZoom)

cRapportSprite : Float
cRapportSprite =3029/1454

cRapportCard : Float
cRapportCard = 256/171

cIntersec : Float
cIntersec = ( cSpriteSize * (3- cRapportCard * cRapportSprite ))/( 2*cRapportSprite*(9-8*cRapportCard) )

cWidth : Float
cWidth = (cSpriteSize - 16 * cIntersec ) / 15
    --round (82*cZoom)

cHeight : Float
cHeight = cRapportCard * cWidth
    --round (118*cZoom)

cStepX : Float
cStepX =cIntersec + cWidth
    --round (92*cZoom)

cStepY : Float
cStepY =cIntersec + cHeight
    --round  (132*cZoom)

cOffsetX : Float
cOffsetX =-cIntersec
    --round (-15*cZoom)


cOffsetY : Float
cOffsetY = -1
    --round (-1*cZoom)


cMargin : Float ----- cacher 25% de la carte
cMargin = (-25/100) * cWidth
-- -20

stackThickness : Int
stackThickness = 6

cardSprite : String
cardSprite =
    pathFilename unoSprite
    --"https://ena.uno/img/ena-sprite.png"


cardPosX : Card -> Float
cardPosX c =
    cOffsetX - toFloat c.value * cStepX


cardPosY : Card -> CardState -> Float
cardPosY c cState =
    if cState == Playabled then
        cOffsetY - toFloat (colorY c.color) * cStepY - cPlayableUp
    else
        cOffsetY - toFloat(colorY c.color) * cStepY


displayStack : Int -> StackShadow -> Card -> Html Msg
displayStack index shadow c =
    let
        z=cZoom
        marginRight=-(90/100) * cWidth
        marginLeft=-(110/100) * cWidth
        i=stackThickness-index
    in
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        , 
            if shadow==Left then
                style "margin-left" (toPx z marginLeft)
            else
                style "margin-right" (toPx z marginRight)

        , style "z-index" (fromInt i)
        ]
        []

displayCard : Card -> CardState -> Html Msg
displayCard c cardState =
    let
        z=cZoom
    in
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
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
displayActionCard c cartTextDiv  =
    let
        z=cZoomAction  
    in
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        --, style "margin-right" (toPx z -cWidth)
        , style "z-index" "100"
        ]
        [cartTextDiv]

displayAvatarCard : Card -> Html Msg
displayAvatarCard c =
    let
        z = cZoomAvatar
    in
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx z cWidth)
        , style "height" (toPx z (cHeight + cPlayableUp))
        , style "background-position-x" (toPx z (cardPosX c))
        , style "background-position-y" (toPx z (cardPosY c Seen))
        , style "background-size" (toPx z cSpriteSize)
        --, style "margin-left" (toPx z cMargin)
        ]
        []

colorY : Color -> Int
colorY color =
    case color of
        Red ->
            0

        Blue ->
            1

        Yellow ->
            2

        Green ->
            3

        Black ->
            4           

playersInit : List Player
playersInit =
    [ { id = 1
      , name = "Doctor Who"
      , hand =[]            
      }
    , { id = 2
      , name = "Tifa FFVII"
      , hand =[]            
      }
    , { id = 3
      , name = "Piccolo"
      , hand =[]            
      }
    ]


notStartedView : Html Msg
notStartedView =
    div []
        [ br [] []
        , h1 [] [ text "UNO" ]
        , button [ onClick RequestedStartGame ]
            [ h3 []
                [ text "  LET'S START...  "
                ]
            ]
        ]

convertIntToColor : Int -> Color
convertIntToColor n =
    case n of
        1 -> Red
        2 -> Blue
        3 -> Yellow
        4 -> Green
        _ -> Black


isHandPlayable : Player -> Card -> Int -> Bool
isHandPlayable player mainCard drawing =
    if mainCard.value==12 && drawing==0 then
        False
    else
        case filter (\h -> h.value == mainCard.value || h.color == mainCard.color) player.hand of
            [] ->
                False

            _ ->
                True


          -- if game.drawing>0 && length game.drawStack > 0 then
            --     case (game.mainCard.value, game.mainCard.color, game.penality) of
            --         (10, _, True) -> --- Si c'est une carte Block
            --             div [] [ text "" ]
            --         (_, _, _) ->
            --             div [] [ displayDrawActionCard game.drawing (length game.drawStack)]
            --   else
            --     div [] [ text "" ]

innerDrawActionCard : Int -> Int -> Html Msg 
innerDrawActionCard  nbDrawCards drawing =
           let
                        displayFlexTop = if drawing>0 then
                                [ text "Draw", br[][] , text ("+" ++ (fromInt drawing) )]
                            else    
                                [ text "  ", br[][], text " " ]

                        cardToBeDisplayed = if drawing>0 then
                                                card_Draw
                                            else
                                                card_DrawNo
           in

                displayActionCard cardToBeDisplayed  
                    (div[style "display" "flex",
                        style "flex-direction" "column",
                        style "height" (toPx cZoomAction cHeight),
                        --style "margin-top" (toPx cZoomAction cPlayableUp),
                        style "color" "YELLOW"
                        ]
                    [
                        div [style "flex" "50%",
                             style "margin-top" "20px",
                             style "font-size" "20px"
                            ]
                            displayFlexTop,
                        div [style "flex" "50%",
                                style "font-size" "13px",
                                style "margin-bottom" "-15px"]
                                [text (fromInt nbDrawCards), br[][], text " cards", br[][], text "left"]
                    ])
            

displayDrawActionCard : Int -> Int -> Int -> Card -> Bool -> Html Msg
displayDrawActionCard drawing nbDrawCards nbDiscardCards masterCard penality =
        let
            cardToBeDisplayed=
                if drawing==0 then
                    if nbDrawCards==0 then
                        "cardToBeDisplayed_DrawEmpty"
                    else
                        "cardToBeDisplayed_DrawNo"
                        
                else
                    if nbDrawCards==0 then
                        if nbDiscardCards>1 then
                            "cardToBeDisplayed_DrawRefill"
                        else
                            "cardToBeDisplayed_DrawEmpty"
                    else
                        if drawing > nbDrawCards then
                            if nbDiscardCards>1 then
                                "cardToBeDisplayed_DrawRefill"
                            else
                                "cardToBeDisplayed_DrawEmpty"
                        else
                            if masterCard.value==10 && masterCard.color/=Black && penality then
                                "cardToBeDisplayed_DrawNo"
                            else
                                "cardToBeDisplayed_Draw"

                    --     if drawing>0 && nbDrawCards>0 then
                    --         if mCardValue==10 && penality then
                    --             False
                    --         else
                    --             True
                    --     else
                    --         False

                    -- cardToBeDisplayed = if drawable then
                    --         card_Draw
                    --     else    
                    --         card_NoDraw

        in
        case cardToBeDisplayed of
            "cardToBeDisplayed_DrawNo" ->
                    displayActionCard card_DrawNo (innerDrawActionCard nbDrawCards drawing) 
            "cardToBeDisplayed_DrawRefill" ->
                a [onClick RefillDrawStack]
                    [displayActionCard card_DrawRefill (div [][])]
            "cardToBeDisplayed_DrawEmpty" ->
                    displayActionCard card_DrawEmpty (div [][]) 
            _ ->
                a [onClick DrawCard,  style "z-index" "100"]
                    [ displayActionCard card_Draw (innerDrawActionCard nbDrawCards drawing) ]

displayBtnEndGame : Html Msg
displayBtnEndGame =
    button [ onClick GameEnded, style "width" "200px" ] [ text "End the game" ]

displayHeaderRight : Game -> Html Msg
displayHeaderRight game =
            div [style "margin-right" "50px"]
                    [ 
                        a [onClick GameEnded, title "End the game !", style "float" "right"]
                        [displayActionCard card_Finish (div[][])]
                    ]


displayHeaderBoard : Game -> Html Msg
displayHeaderBoard game = 
    div[ style "display" "flex"][
        div[style "flex" "3"][ displayHeaderMaster game ],
        div[style "flex" "6" ][
                displayPlayerName game,
                div [style "display" "flex"][
                    div[style "flex" "6", style "display" "flex", style "justify-content" "center"]
                    (
                        map (\(i,c)-> displayStack i Right c ) 
                            <| take stackThickness
                            <| indexedMap Tuple.pair game.discardStack
                    ),
                    div[style "flex" "6",
                    style "display" "flex",
                    style "justify-content" "center"
                    ]
                                (  
                              (
                                  displayDrawActionCard game.drawing (length game.drawStack) (length game.discardStack) game.mainCard game.penality
                              )  
                              :: (
                                  map (\(i,c)-> displayStack i Left card_Back ) 
                                    <| take stackThickness
                                    <| indexedMap Tuple.pair game.drawStack)
                                )
                    
                    ]
                ],
        div[style "flex" "3"][ displayHeaderRight game]
    ]

displayPlayerName : Game -> Html Msg
displayPlayerName game =
                div [style "background-color" "YELLOW"
                ]
                [ 
                    text (headPlayer game.players).name
                ]

displayHeaderMaster : Game -> Html Msg
displayHeaderMaster game = 
          div []
            [ 
            -- pour PLAY
            -- il faut Jouable
            -- , if isHandPlayable (headPlayer game.players) game.mainCard game.drawing then
            --     b [] [ text "Play a card" ]

            --   else
            --     b [] [ text "You can't play any card" ]

            -- pour PASS
            -- il faut Piocher
            ------> Fonctionnalité déporté vers displayPlayer
            -- -- if game.drawing==0 || (game.mainCard.value==10 && game.penality==True) then
            -- --     div [] [ displayPassActionCard ]
            -- --   else
            -- --     div [] [ text "" ]

            -- pour PIOCHER
            -- il faut Pas Déjà Piocher
            -- il faut Draw plein
            -- if game.drawing>0 && length game.drawStack > 0 then
            --     case (game.mainCard.value, game.mainCard.color, game.penality) of
            --         (10, _, True) -> --- Si c'est une carte Block
            --             div [] [ text "" ]
            --         (_, _, _) ->
            --             div [] [ displayDrawActionCard game.drawing (length game.drawStack)]
            --   else
            --     div [] [ text "" ]

            


            -- pour FILL
            -- il faut Deck>=2 cartes
            -- il faut Draw vide
            -- il faut Pas déjà Piocher
             if length game.drawStack == 0 && length game.discardStack > 1 && game.drawing>0 then
                div [] [ text "Click on the DRAW pile to regenerate it." ]

              else
                div [] [text "" ]

            -- pour End Game
            -- il faut Pas Jouable
            -- il faut Draw vide
            -- il faut Deck <=1 carte
            -- il faut Pas déjà Piocher
            , if
                not (isHandPlayable (headPlayer game.players) game.mainCard game.drawing)
                    && length game.drawStack == 0
                    && length game.discardStack <= 1
                    && game.drawing>0
              then
                div [] [ displayBtnEndGame ]

              else
                div [] [ text "" ]
            , div [ style "background-color" "LIGHTYELLOW" ]
                [ if length game.drawStack == 0 then
                    h4 []
                        [ text "The deck is empty"
                        , if length game.discardStack <= 1 && game.drawing>0 then
                            div []
                                [ text "there's not any card left."
                                ]

                          else
                            text ""
                        ]

                  else
                    div [] [ text "" ]
--                    h4 [] [ text (fromInt (length game.drawStack)), text " cards left in the deck " ]
                ]
            ,
                if game.penality && game.mainCard.value==14 && game.mainCard.color==Black then
                    div[][text "Choose your color", 
                        div[style "display" "flex", style "justify-content" "center"]
                        (    map (\i ->
                                    a [onClick (SetWildCardColor (convertIntToColor i)) ][
                                        displayCard {id=0, value=14, color=convertIntToColor i } Seen
                                    ])
                            (range 1 4)
                        )
                    ]
                else
                    div[][text ""]

            ]


playingView : Game -> Html Msg
playingView game =
    div [ style "background-color" "AZURE" ]
        [ 
         displayHeaderBoard game

        , displayPlayers game.players game.mainCard game.drawing game.penality game.reverse
        -- , div [ style "background-color" "LIGHTYELLOW" ]
        --     [ h3 [ style "background-color" "YELLOW", style "padding" "10px" ] [ text "GAME BOARD" ]
        --     , div [ style "overflow" "hidden", style "padding-left" "70px" ] [ displayCard (firstCard game.discardStack) Seen ]
        --     , div [ style "clear" "left" ] [ displayCards (omitCard (firstCard game.discardStack) game.discardStack) NotPlayabled ]
        --     ]
        , hr [] []
        , div []
            [ displayBtnEndGame
            , h4 [] [ text "UNO - Workshop ELM - October 2019" ]
            ]
        ]

--------------- DEFAULT 
noPlayer : Player
noPlayer =
    { id = 0
    , name = "NoBody"
    , hand = []
    }

card_Pass : Card 
card_Pass =
    { id = 0
    , value = 2
    , color = Black
    }

card_Back : Card
card_Back =
    { id = 0
    , value = 0
    , color = Black
    }

card_Draw : Card
card_Draw =
    { id = 0
    , value = 1
    , color = Black
    }

card_DrawNo : Card
card_DrawNo =
    { id = 0
    , value = 2
    , color = Black
    }

card_Finish : Card
card_Finish =
    { id = 0
    , value = 3
    , color = Black
    }

card_DrawEmpty : Card
card_DrawEmpty =
    { id = 0
    , value = 4
    , color = Black
    }

card_DrawRefill : Card
card_DrawRefill =
    { id = 0
    , value = 5
    , color = Black
    }

card_Player : Int -> Card
card_Player n =
    { id = 0
    , value = 9+n
    , color = Black
    }

card_NextUp : Card
card_NextUp =
    { id = 0
    , value = 8
    , color = Black
    }

card_NextDown : Card
card_NextDown =
    { id = 0
    , value = 9
    , color = Black
    }


--------------- DEFAULT 

headPlayer : List Player -> Player
headPlayer listPlayer =
    case head listPlayer of
        Just player ->
            player
        Nothing ->
            noPlayer

tailPlayer : List Player -> List Player
tailPlayer listPlayer =
    case tail listPlayer of
        Just players ->
            players
        Nothing ->
            []

displayInfoHand : List Card -> Card -> Html Msg
displayInfoHand hand mainCard =
    if mainCard == card_Back then
        text ((fromInt <| nbPointInHand hand) ++ " points")

    else
        text ((fromInt <| nbCardInHand hand) ++ " cards - " ++ (fromInt <| nbPointInHand hand) ++ " points")

displayNextCard : Player -> Card -> Player -> Int -> Bool -> Reverse -> Html Msg
displayNextCard player mainCard currentPlayer drawing penality reverse =
        let 
            cardNextToBeDisplayed =
                if reverse==ToRight then
                    card_NextDown
                else
                    card_NextUp
        in    
        if (currentPlayer == player) then
            if drawing==0 || (mainCard.value==10 && penality==True) then
                a [onClick Pass][ displayCard cardNextToBeDisplayed Seen ]
            else
                div[ style "filter" "contrast(50%)" ][displayCard cardNextToBeDisplayed Seen]
        else
            text ""


displayPlayers : List Player -> Card -> Int -> Bool -> Reverse -> Html Msg
displayPlayers players mainCard drawing penality reverse =
    div [] (
        map (\p -> displayPlayer p mainCard (headPlayer players) drawing penality reverse)
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
            [
                style "display" "flex",
                let 
                    isCurrentPlayer=currentPlayer==player
                in
                case (isCurrentPlayer, penality) of
                    
                    (True, True) ->
                            style "background-color" "RED"
                    
                    (True, False) ->
                            style "background-color" "GREEN"
                    
                    (False , _ ) ->
                            style "background-color" "WHITE"
            ]
            [ 
                div[style "flex" "1", style "margin-bottom" "10px", style "margin-left" "50px"][ displayAvatarCard (card_Player player.id) ],
                div[style "flex" "10", style "margin-top" "10px", style "margin-bottom" "10px",
                        style "display" "flex",
                        style "justify-content" "flex-end",
                        style "flex-direction" "column"
                
                ][
                                 if currentPlayer == player then
                                    displayHandCards player.hand mainCard drawing penality
                                else
                                    displayCards player.hand NotPlayabled
                ],
                div[style "flex" "1", style "margin-top" "10px", style "margin-bottom" "10px"]
                    [
                    displayNextCard player mainCard currentPlayer drawing penality reverse 
                    ]

            ]
        ]


nbCardInHand : List Card -> Int
nbCardInHand cards =
    length cards


nbPointInHand : List Card -> Int
nbPointInHand cards =
    sum <| map (\c -> c.value) cards


firstCard : List Card -> Card
firstCard listCard =
    case head listCard of
        Just card ->
            card

        Nothing ->
            card_Back


displayPlayabled : Card -> Html Msg
displayPlayabled h = a [ onClick (CardPlayed h) ] [ displayCard h Playabled ]

displayNotPlayabled : Card -> Html Msg
displayNotPlayabled h = displayCard h NotPlayabled

displayRegular : Card -> Card -> Html Msg
displayRegular h mainCard =
    if h.value==mainCard.value || h.color==mainCard.color || h.color==Black then
        displayPlayabled h
    else
        displayNotPlayabled h

displayHandCards : List Card -> Card -> Int -> Bool -> Html Msg
displayHandCards cards mainCard drawing penality =
    div [ style "margin-left" (toPx 1 -cMargin)]
        (map
            (\h ->
                case (mainCard.value, mainCard.color, drawing) of
                    (12, _ ,0 ) -> 
                        if penality then
                            displayNotPlayabled h    
                        else
                            displayRegular h mainCard
                    (12, _ ,1 ) -> 
                            displayRegular h mainCard
                    (12, _ ,_ ) -> 
                        if h.value==12 || (h.color==Black && h.value==13) then --- [+2] ou [+4] jouables
                            displayPlayabled h
                        else
                            displayNotPlayabled h

                    (13, Black, 0) -> 
                        if penality then
                            displayNotPlayabled h    
                        else
                            displayRegular h mainCard
                    (13, Black ,1 ) -> 
                            displayRegular h mainCard
                    (13, Black ,_ ) -> 
                        if h.color==Black && h.value==13 then --- [+2] ou [+4] jouables
                            displayPlayabled h
                        else
                            displayNotPlayabled h
                    (10, _ ,_ ) ->
                        if penality then
                            displayNotPlayabled h    
                        else
                            displayRegular h mainCard
                           

                    (_,_,_) ->
                        displayRegular h mainCard

   

            )
            cards
        )


displayCards : List Card -> CardState -> Html Msg
displayCards cards cardState =
        div [ style "margin-left" (toPx 1 -cMargin)] (map (\c -> displayCard c cardState) cards)


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

        Black ->
            "Black"


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
                    text (winner.name ++ " with " ++ fromInt winner.pts ++ " points")

            Nothing ->
                text "Nobody wins"
        ]


gameOver : List Player -> Html Msg
gameOver players =
    div []
        [ h1 [] [ text "GAME OVER" ]
        , hr [] []
        , h2 [] [ text "The winner is " ]
        , h2 [] [ displayWinner players ]
        , hr [] []
        , displayPlayers players card_Back 0 False ToRight
        ]


reverseDirection : List Player -> Reverse -> List Player
reverseDirection listPlayers sens =
    case sens of
        ToRight -> 
            reverse (headPlayer listPlayers :: reverse (tailPlayer listPlayers))
        ToLeft -> 
            headPlayer (reverse listPlayers) :: reverse (tailPlayer (reverse listPlayers))
        ToStay ->
            listPlayers

omitCard : Card -> List Card -> List Card
omitCard cardToOmit listCards =
    filter (\c -> c /= cardToOmit) listCards


omitPlayedCard : Card -> List Player -> List Player
omitPlayedCard cardToOmit allPlayers =
    map (\p -> { p | hand = omitCard cardToOmit p.hand }) allPlayers


drawCardToPlayer : List Player -> List Card -> Int -> List Player
drawCardToPlayer players cards drawing =
    map
        (\p ->
            { p
                | hand =
                    if (headPlayer players).id == p.id then
                        reverse (append (take drawing cards) (reverse p.hand))

                    else
                        p.hand
            }
        )
        players


tailCard : List Card -> List Card
tailCard cards =
    case tail cards of
        Just c ->
            c

        Nothing ->
            []


hasWinner : List Player -> Card -> Bool
hasWinner players lastCardPlayed =
    players
        |> filter (\p -> isEmpty (omitCard lastCardPlayed p.hand))
        |> isEmpty
        |> not

nbBlacks : Int
nbBlacks = 8

nbColors : Int
nbColors = 4

nbCardsByColor : Int
nbCardsByColor = 26 -- (0 à 9 + double + Sens + Pass)*2

nbCards : Int
nbCards = nbColors * nbCardsByColor + nbBlacks

nbPlayers : Int
nbPlayers = 3

nbCardsByPlayer : Int
nbCardsByPlayer = 7

newIndicesGenerator : Random.Generator (List Int)
newIndicesGenerator = 
    Random.list nbCards (Random.int 1 1000)


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            notStartedView

        Playing game ->
            playingView game

        GameOver players ->
            gameOver players


initCardAddIndice : Card -> Int -> Card
initCardAddIndice c i = 
    { c | id=i }

initShuffleCards : List Card -> List Int -> List Card
initShuffleCards cards generatedNewIds =
    sortBy .id (map2 (initCardAddIndice) cards generatedNewIds)

initHandOfPlayers : List Card -> List Player -> List Player
initHandOfPlayers cards players =
    map (\p -> { p | hand = take nbCardsByPlayer ( drop (nbCardsByPlayer * (p.id-1)) cards) }) players

getReverse : Int -> Reverse -> Reverse
getReverse value sens =
    if value==11 then
        case sens of
            ToRight ->
                ToLeft
            ToLeft -> 
                ToRight
            ToStay -> 
                ToStay
    else
        sens

getNumberDrawing : Card -> Int -> Int
getNumberDrawing c drawing =
    case c.value of
        12 -> ---- si c'est un [+2]
            if drawing==1 then ------ drawing init pas utilsé
                2   ----------------- On applique la pénalité au suivant [+2]
            else
                drawing + 2 --------- une pénalité existe déjà, on y ajoute [+2]
        13 -> 
            if c.color==Black then ---- On a joué un [+4]
                if drawing==1 then ------ drawing init pas utilsé
                    4   ----------------- On applique la pénalité au suivant [+4]
                else
                    drawing + 4 --------- une pénalité existe déjà, on y ajoute [+4]
            else
                1 ---- C'est un [1] Color normal -> On init le drawing à 1
        _ -> 1 ------ On init le drawing à 1

setValue : Int -> Card -> Card
setValue v card =
    {
        id=card.id,
        value=v,
        color=card.color
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case ( msg, model ) of

        (DemandeNewListCards, _ ) ->
            ( model, Random.generate DistributeDrawStack newIndicesGenerator)

        (DistributeDrawStack generatedNewIds, Playing game )  ->
            let 
                shuffleCards=initShuffleCards drawStackInit generatedNewIds
            in
            (Playing
                { game | 
                  originStack = drawStackInit
                , mainCard = firstCard shuffleCards
                , players = initHandOfPlayers ( drop 1 shuffleCards) game.players
                , drawStack = take 3 (drop (nbCardsByPlayer * nbPlayers + 1) shuffleCards)
                , discardStack = firstCard shuffleCards :: []
                }
                , Cmd.none)
     
        ( RequestedStartGame, _ ) ->
            (Playing
                {
                  originStack = []
                , mainCard = card_Back
                , reverse = ToRight  
                , players = playersInit
                , drawStack = []
                , discardStack =  []
                , drawing = 1
                , penality = False
                }
                , Random.generate DistributeDrawStack newIndicesGenerator)

        ( CardPlayed cardPlayed, Playing game ) ->
            let
                let_OmitPlayedCard=omitPlayedCard cardPlayed game.players
            in
            if hasWinner game.players cardPlayed then
                (GameOver let_OmitPlayedCard
                 , Cmd.none    
                )

            else
                if cardPlayed.value==14 && cardPlayed.color==Black then
                        (Playing
                            { game |
                                players = 
                                    reverseDirection 
                                    let_OmitPlayedCard
                                    ToStay
                                , discardStack = cardPlayed :: game.discardStack
                                , mainCard = cardPlayed
                                , penality = True
                            }
                        , Cmd.none    
                        )
                else
                        let
                            let_Reverse = getReverse cardPlayed.value game.reverse
                        in
                        (Playing
                            { game |
                                reverse = let_Reverse
                                , players = 
                                        reverseDirection 
                                            let_OmitPlayedCard
                                            let_Reverse
                                , discardStack = cardPlayed :: game.discardStack
                                , drawing = getNumberDrawing cardPlayed game.drawing
                                , mainCard = cardPlayed
                                , penality = (cardPlayed.value==12 || (cardPlayed.value==13 && cardPlayed.color==Black) || cardPlayed.value==10)
                            }
                        , Cmd.none    
                        )
        
        ( DrawCard, Playing game ) ->
            (Playing
                { game
                    | players = drawCardToPlayer game.players game.drawStack game.drawing
                    , drawStack = drop game.drawing game.drawStack
                    , drawing = 0
                    --, penality = False
                    --, mainCard = setValue -1 game.mainCard
                }
             , Cmd.none    
            )

        ( RefillDrawStack, Playing game ) ->
            (Playing
                { game
                    | drawStack = tailCard game.discardStack
                    , discardStack = firstCard game.discardStack :: []
                }
            , Cmd.none    
            )

        ( Pass, Playing game ) ->
            (Playing
                { game
                    | players = reverseDirection game.players game.reverse
                    , drawing = 1
                    , penality = False
                }
             , Cmd.none    
            )

        (SetWildCardColor color, Playing game) ->
            let
                coloredWildCard = game.mainCard
            in
            (Playing
                { game |
                    players = 
                            reverseDirection 
                                game.players
                                game.reverse
                    , discardStack = { coloredWildCard | color=color} :: tailCard game.discardStack
                    , drawing = getNumberDrawing game.mainCard game.drawing
                    , mainCard = { coloredWildCard | color=color}
                    , penality = False
                }
            , Cmd.none    
            )

        ( DoNothing, _ ) ->
            (model, Cmd.none)

        ( GameEnded, Playing game ) ->
            (GameOver game.players, Cmd.none)



        ( _, _ ) ->
            (model, Cmd.none)


-- SUBSCRIPTIONS


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