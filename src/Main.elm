module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, h4, h5, hr, span, text, img)
import Html.Attributes exposing (style, title)
import Html.Events exposing (onClick)
import List exposing (filter, head, isEmpty, length, map, reverse, sortBy, sum, tail, repeat, range, map2, take, drop, concat, append )
import String exposing (fromInt)
import Basics exposing (round)
import Random
import Asset exposing (src, unoSprite, arrowUp, arrowDown, Image, path, pathFilename)

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
    | Back


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

type PermutationSens = ToRight | ToLeft

type alias Game =
    { 
      originStack : List Card
    , mainCard : Card
    , permutationSens : PermutationSens
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

buildZero : List Card
buildZero =
        map (\c -> { id=0, value=0, color=convertIntToColor(c)} ) (range 1 4)

buildBlack : List Card
buildBlack = 
    concat (
        map (\_ -> 
            map (\v -> { id=0, value=v, color=Black}) (range 1 2)
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

toPx : Float -> Int -> String
toPx zoom v =
    fromInt (round ((toFloat v)*zoom)) ++ "px"

cZoomAction : Float
cZoomAction=1.0

cZoom : Float
cZoom = 0.8

cPlayableUp : Int
cPlayableUp =13
    --round(13*cZoom)


cStepX : Int
cStepX =92
    --round (92*cZoom)


cStepY : Int
cStepY =132
    --round  (132*cZoom)

cWidth : Int
cWidth =82
    --round (82*cZoom)


cHeight : Int
cHeight =118
    --round (118*cZoom)

cSpriteSize : Int
cSpriteSize =1216
    --round (1216*cZoom)

cOffsetX : Int
cOffsetX =-15
    --round (-15*cZoom)


cOffsetY : Int
cOffsetY =-1
    --round (-1*cZoom)

cMargin : Int
cMargin = -20



cardSprite : String
cardSprite =
    pathFilename unoSprite
    --"https://ena.uno/img/ena-sprite.png"


cardPosX : Card -> Int
cardPosX c =
    cOffsetX - c.value * cStepX


cardPosY : Card -> CardState -> Int
cardPosY c cState =
    if cState == Playabled then
        cOffsetY - colorY c.color * cStepY - cPlayableUp

    else
        cOffsetY - colorY c.color * cStepY


displayCard : Card -> CardState -> Html Msg
displayCard c cardState =
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx cZoom cWidth)
        , style "height" (toPx cZoom (cHeight + cPlayableUp))
        , style "background-position-x" (toPx cZoom (cardPosX c))
        , style "background-position-y" (toPx cZoom (cardPosY c cardState))
        , style "float" "left"
        , style "background-size" (toPx cZoom cSpriteSize)
        , style "margin-left" (toPx cZoom cMargin)
        ]
        []

displayActionCard : Card -> String -> Html Msg
displayActionCard c cartText  =
    div
        [ style "background-image" ("url(" ++ cardSprite ++ ")")
        , style "width" (toPx cZoomAction cWidth)
        , style "height" (toPx cZoomAction (cHeight + cPlayableUp))
        , style "background-position-x" (toPx cZoomAction (cardPosX c))
        , style "background-position-y" (toPx cZoomAction (cardPosY c Seen))
        , style "background-size" (toPx cZoomAction cSpriteSize)
        , style "float" "left"
        ]
        [  div[ 
            style "margin-top" "10px" , style "color" "RED" ][text cartText]]

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
        
        Back ->
            4


playersInit : List Player
playersInit =
    [ { id = 1
      , name = "Wing"
      , hand =[]            
      }
    , { id = 2
      , name = "Theo"
      , hand =[]            
      }
    , { id = 3
      , name = "Alex"
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
        5 -> Black
        _ -> Back


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


displayBtnDraw : Int -> Html Msg
displayBtnDraw drawing =
    button [ onClick DrawCard, style "width" "200px" ] [ text ("Draw " ++ fromInt(drawing)++ " cards") ]


displayBtnPass : Html Msg
displayBtnPass =
    button [ onClick Pass, style "width" "200px" ] [ text "Pass" ]


displayBtnFillDraw : Html Msg
displayBtnFillDraw =
    button [ onClick RefillDrawStack, style "width" "200px" ] [ text "Fill the deck" ]


displayBtnEndGame : Html Msg
displayBtnEndGame =
    button [ onClick GameEnded, style "width" "200px" ] [ text "End the game" ]


displayArrow : PermutationSens -> Image
displayArrow permutationSens =
    case permutationSens of
        ToRight -> arrowDown
        ToLeft -> arrowUp

displayHeaderActionCards : Game -> Html Msg
displayHeaderActionCards game =
            div [ style "margin-left" "50px"]
                    [ displayActionCard (firstCard game.discardStack) ""
                    , a [onClick DrawCard, title "Draw cards here !"]
                        [displayActionCard noCard ""]
                    ]

displayHeaderInfoCards : Game -> Html Msg
displayHeaderInfoCards game =
            div [style "margin-right" "50px"]
                    [ 
                        a [onClick GameEnded, title "End the game !", style "float" "right"]
                        [displayActionCard {noCard | value=2} "Game Over"],
                        
                        div[style "float" "right"][img [src (displayArrow game.permutationSens),
                                        style "width" (toPx cZoomAction cWidth), style "height" (toPx cZoomAction cHeight), style "margin-top" (toPx cZoomAction cPlayableUp)][ ]
                            ]
                    
                    ]


displayHeaderBoard : Game -> Html Msg
displayHeaderBoard game = 
    div[ style "display" "flex"][
        div[style "flex" "3"][ displayHeaderActionCards game],
        div[style "flex" "6"  ][ displayHeaderMaster game],
        div[style "flex" "3"][ displayHeaderInfoCards game]
    ]

displayHeaderMaster : Game -> Html Msg
displayHeaderMaster game = 
          div []
            [ div
                [ style "padding" "2px"
                , style "background-color" "LIGHTBLUE"
                ]
                [ b [ style "font-size" "20px" ] [ 
                    text (headPlayer game.players).name
                    ]
                ]

            -- pour PLAY
            -- il faut Jouable
            -- , if isHandPlayable (headPlayer game.players) game.mainCard game.drawing then
            --     b [] [ text "Play a card" ]

            --   else
            --     b [] [ text "You can't play any card" ]

            -- pour PASS
            -- il faut Piocher
            , if game.drawing==0 || (game.mainCard.value==10 && game.penality==True) then
                div [] [ displayBtnPass ]

              else
                div [] [ text "" ]

            -- pour PIOCHER
            -- il faut Pas Déjà Piocher
            -- il faut Draw plein
            , if game.drawing>0 && length game.drawStack > 0 then
                case (game.mainCard.value, game.mainCard.color, game.penality) of
                    (10, _, True) ->
                        div [] [ text "" ]
                    (_, _, _) ->
                        div [] [ displayBtnDraw game.drawing]
              else
                div [] [ text "" ]

            -- pour FILL
            -- il faut Deck>=2 cartes
            -- il faut Draw vide
            -- il faut Pas déjà Piocher
            , if length game.drawStack == 0 && length game.discardStack > 1 && game.drawing>0 then
                div [] [ displayBtnFillDraw ]

              else
                div [] [ text "" ]

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
                                [ text "and there's not any card left."
                                ]

                          else
                            text ""
                        ]

                  else
                    h4 [] [ text (fromInt (length game.drawStack)), text " cards left in the deck " ]
                ]
            ]


playingView : Game -> Html Msg
playingView game =
    div [ style "background-color" "AZURE" ]
        [ 
         displayHeaderBoard game

        , displayPlayers game.players (game.mainCard) game.drawing game.penality
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


noPlayer : Player
noPlayer =
    { id = 0
    , name = "NoBody"
    , hand = []
    }


noCard : Card
noCard =
    { id = 0
    , value = 0
    , color = Back
    }

emptyCard : Card
emptyCard =
    { id = 0
    , value = 4
    , color = Back
    }

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
    if mainCard == noCard then
        text ((fromInt <| nbPointInHand hand) ++ " points")

    else
        text ((fromInt <| nbCardInHand hand) ++ " cards - " ++ (fromInt <| nbPointInHand hand) ++ " points")


displayPlayers : List Player -> Card -> Int -> Bool -> Html Msg
displayPlayers players mainCard drawing penality =
    div [] (map (\p -> displayPlayer p mainCard (headPlayer players) drawing penality) (sortBy .id players)) 


displayPlayer : Player -> Card -> Player -> Int -> Bool -> Html Msg
displayPlayer player mainCard currentPlayer drawing penality =
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
            [ if currentPlayer == player then
                if penality then 
                    style "background-color" "RED"
                else
                    style "background-color" "GREEN"
              else
                style "background-color" "WHITE"
            ]
            [ br [] []
            , if currentPlayer == player then
                displayHandCards player.hand mainCard drawing penality

              else
                displayCards player.hand NotPlayabled
            , br [] []
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
            noCard


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
    div [ style "overflow" "hidden", style "padding-left" "70px" ]
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
                        if h.value==12 || (h.color==Black && h.value==1) then --- [+2] ou [+4] jouables
                            displayPlayabled h
                        else
                            displayNotPlayabled h

                    (1, Black, 0) -> 
                        if penality then
                            displayNotPlayabled h    
                        else
                            displayRegular h mainCard
                    (1, Black ,1 ) -> 
                            displayRegular h mainCard
                    (1, Black ,_ ) -> 
                        if h.color==Black && h.value==1 then --- [+2] ou [+4] jouables
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
    div [ style "overflow" "hidden", style "padding-left" "70px" ] (map (\c -> displayCard c cardState) cards)


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

        Back ->
            "Back"


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
        , displayPlayers players noCard 0 False
        ]


permutePlayer : List Player -> PermutationSens -> List Player
permutePlayer listPlayers sens =
    case sens of
        ToRight -> 
            reverse (headPlayer listPlayers :: reverse (tailPlayer listPlayers))
        ToLeft -> 
            headPlayer (reverse listPlayers) :: reverse (tailPlayer (reverse listPlayers))

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

getPermutationSens : Int -> PermutationSens -> PermutationSens
getPermutationSens value sens =
    if value==11 then
        case sens of
            ToRight -> ToLeft
            ToLeft -> ToRight
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
        1 -> 
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
                  --originStack = initShuffleCards drawStackInit generatedNewIds
                  originStack = drawStackInit
                , mainCard = firstCard shuffleCards
                , players = initHandOfPlayers ( drop 1 shuffleCards) game.players
                , drawStack = drop (nbCardsByPlayer * nbPlayers + 1) shuffleCards
                , discardStack = firstCard shuffleCards :: []
                }
                , Cmd.none)
     
        ( RequestedStartGame, _ ) ->
            (Playing
                {
                  originStack = []
                , mainCard = noCard
                , permutationSens = ToRight  
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
                let
                    let_PermutationSens = getPermutationSens cardPlayed.value game.permutationSens
                in
                (Playing
                    { game |
                        permutationSens = let_PermutationSens
                        , players = 
                          permutePlayer 
                            let_OmitPlayedCard
                            let_PermutationSens
                        , discardStack = cardPlayed :: game.discardStack
                        , drawing = getNumberDrawing cardPlayed game.drawing
                        , mainCard = cardPlayed
                        , penality = (cardPlayed.value==12 || (cardPlayed.value==1 && cardPlayed.color==Black) || cardPlayed.value==10)
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
                    | players = permutePlayer game.players game.permutationSens
                    , drawing = 1
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