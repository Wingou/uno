module Functions exposing (..)

import Constants exposing (..)
import List exposing (append, concat, drop, filter, head, isEmpty, length, map, map2, range, reverse, sortBy, sum, tail, take)
import Random
import String exposing (fromInt)
import Types exposing (..)


textWord : Int -> String -> String
textWord n word =
    if n >= 2 then
        word ++ "s"

    else
        word


textNbWords : Int -> String -> String
textNbWords n word =
    fromInt n ++ " " ++ textWord n word


nbCardInHand : List Card -> Int
nbCardInHand cards =
    length cards


nbPointInHand : List Card -> Int
nbPointInHand cards =
    sum <| map (\c -> c.value) cards


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


getReverse : Int -> Reverse -> Reverse
getReverse value sens =
    if value == 11 then
        case sens of
            ToRight ->
                ToLeft

            ToLeft ->
                ToRight

            ToStay ->
                ToStay

    else
        sens


getDrawing : Card -> Int -> Int
getDrawing c drawing =
    case c.value of
        12 ->
            ---- si c'est un [+2]
            if drawing == 1 then
                ------ drawing init pas utilsé
                2
                ----------------- On applique la pénalité au suivant [+2]

            else
                drawing + 2

        --------- une pénalité existe déjà, on y ajoute [+2]
        13 ->
            if c.color == Black then
                ---- On a joué un [+4]
                if drawing == 1 then
                    ------ drawing init pas utilsé
                    4
                    ----------------- On applique la pénalité au suivant [+4]

                else
                    drawing + 4
                --------- une pénalité existe déjà, on y ajoute [+4]

            else
                1

        ---- C'est un [1] Color normal -> On init le drawing à 1
        10 ->
            0

        ------------------- Si c'est un SKIP, on ne peut pas draw
        _ ->
            1



------ On init le drawing à 1


omitPlayedCard : Card -> List Player -> List Player
omitPlayedCard cardToOmit allPlayers =
    map (\p -> { p | hand = omitCard cardToOmit p.hand }) allPlayers


avatarToBeSet : List Player -> Player
avatarToBeSet listPlayers =
    case head <| filter (\p -> p.name == "noname") <| listPlayers of
        Just p ->
            p

        Nothing ->
            noPlayer


initHandOfPlayers : List Card -> List Player -> List Player
initHandOfPlayers cards players =
    map (\p -> { p | hand = take nbCardsByPlayer (drop (nbCardsByPlayer * (p.id - 1)) cards) }) players


getCard : Int -> Color -> Card
getCard v c =
    { id = 0
    , value = v
    , color = c
    }


reverseDirection : List Player -> Reverse -> List Player
reverseDirection listPlayers sens =
    case sens of
        ToRight ->
            reverse (headPlayer listPlayers :: reverse (tailPlayer listPlayers))

        ToLeft ->
            headPlayer (reverse listPlayers) :: reverse (tailPlayer (reverse listPlayers))

        ToStay ->
            listPlayers


tailPlayer : List Player -> List Player
tailPlayer listPlayer =
    case tail listPlayer of
        Just players ->
            players

        Nothing ->
            []


headPlayer : List Player -> Player
headPlayer listPlayer =
    case head listPlayer of
        Just player ->
            player

        Nothing ->
            noPlayer


tailCard : List Card -> List Card
tailCard cards =
    case tail cards of
        Just c ->
            c

        Nothing ->
            []


newIndicesGenerator : Random.Generator (List Int)
newIndicesGenerator =
    Random.list nbCards (Random.int 1 1000)


nbCards : Int
nbCards =
    nbColors * nbCardsByColor + nbBlacks


initShuffleCards : List Card -> List Int -> List Card
initShuffleCards cards generatedNewIds =
    sortBy .id (map2 initCardAddIndice cards generatedNewIds)


initCardAddIndice : Card -> Int -> Card
initCardAddIndice c i =
    { c | id = i }


drawStackInit : List Card
drawStackInit =
    buildZero
        ++ buildRegular
        ++ buildBlack



--- Build 96 --> 2 x 4 couleurs x (de 1 à 12 <=> 1, 2, ..., 9 + 3 Spé )


buildRegular : List Card
buildRegular =
    concat
        (map
            (\_ ->
                concat
                    (map
                        (\c ->
                            map (\v -> { id = 0, value = v, color = convertIntToColor c }) (range 1 12)
                        )
                        (range 1 4)
                    )
            )
            (range 1 2)
        )



--- Build 4 --> 4 couleurs de la carte 0


buildZero : List Card
buildZero =
    map (\c -> { id = 0, value = 0, color = convertIntToColor c }) (range 1 4)



--- Build 8 --> 4 x les 2 jokers


buildBlack : List Card
buildBlack =
    concat
        (map
            (\_ ->
                map (\v -> { id = 0, value = v, color = Black }) (range 13 14)
            )
            (range 1 4)
        )


convertIntToColor : Int -> Color
convertIntToColor n =
    case n of
        1 ->
            Red

        2 ->
            Blue

        3 ->
            Yellow

        4 ->
            Green

        _ ->
            Black


firstCard : List Card -> Card
firstCard listCard =
    case head listCard of
        Just card ->
            card

        Nothing ->
            card_Back


omitCard : Card -> List Card -> List Card
omitCard cardToOmit listCards =
    filter (\c -> c /= cardToOmit) listCards


hasWinner : List Player -> Card -> Bool
hasWinner players lastCardPlayed =
    players
        |> filter (\p -> isEmpty (omitCard lastCardPlayed p.hand))
        |> isEmpty
        |> not


isHandPlayable : Player -> Card -> Int -> Bool -> Bool
isHandPlayable player mainCard drawing penality =
    if ((mainCard.value == 12 && mainCard.color /= Black) || mainCard.value == 13 || (mainCard.value == 10 && mainCard.color /= Black)) && penality then
        False

    else if length (filter (\h -> h.value == mainCard.value || h.color == mainCard.color) player.hand) > 0 then
        True

    else if drawing == 0 then
        False

    else
        True


toPx : Float -> Float -> String
toPx zoom v =
    fromInt (round (v * zoom)) ++ "px"


cardPosX : Card -> Float
cardPosX c =
    cOffsetX - toFloat c.value * cStepX


cardPosY : Card -> CardState -> Float
cardPosY c cState =
    if cState == Playabled then
        cOffsetY - toFloat (colorY c.color) * cStepY - cPlayableUp

    else
        cOffsetY - toFloat (colorY c.color) * cStepY


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


card_Player : Int -> Card
card_Player n =
    { id = 0
    , value = n
    , color = Red
    }
