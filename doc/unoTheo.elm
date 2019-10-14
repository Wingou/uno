
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Color
    = Red
    | Green
    | Blue
    | Yellow


type alias Card =
    { value : Int
    , color : Color
    }


type alias Player =
    { id : String
    , name : String
    , hand : List Card
    }


type alias Game =
    { players : List Player
    , drawStack : List Card
    , discardStack : List Card
    }


type Model
    = NotStarted
    | Playing Game
    | GameOver Player


type Msg
    = RequestedStartGame
    | PlayedCard Card
    | TookFromDrawStack


colorToString : Color -> String
colorToString c =
    case c of
        Red ->
            "Red"

        Green ->
            "Green"

        Blue ->
            "Blue"

        Yellow ->
            "Yellow"


canPlay : Card -> Card -> Bool
canPlay cardToPlay card =
    cardToPlay.color == card.color || cardToPlay.value == card.value


cardToString : Card -> String
cardToString { value, color } =
    String.fromInt value ++ " " ++ colorToString color


buildNumberCard : Color -> Int -> Card
buildNumberCard color num =
    Card num color


buildDeckByColor c =
    List.range 0 9
        |> List.map (buildNumberCard c)


buildDeck : List Card
buildDeck =
    []
        |> List.append (buildDeckByColor Red)
        |> List.append (buildDeckByColor Yellow)
        |> List.append (buildDeckByColor Green)
        |> List.append (buildDeckByColor Blue)


initializeGame : Game
initializeGame =
    let
        allCards =
            buildDeck

        players =
            [ { id = "1"
              , name = "Wing"
              , hand = []
              }
            , { id = "2"
              , name = "Alex"
              , hand = []
              }
            ]

        playerWithHands =
            players
                |> List.indexedMap
                    (\i p ->
                        let
                            remainingCards =
                                allCards |> List.drop (i * 7)
                        in
                        { p | hand = remainingCards |> List.take 7 }
                    )
    in
    { players = playerWithHands
    , drawStack = allCards
    , discardStack = []
    }


initialModel : Model
initialModel =
    Playing
        initializeGame


notStartedView =
    Html.button [ onClick RequestedStartGame ] [ text "Press to start ..." ]


cardsView : List Card -> Bool -> Maybe Card -> Html Msg
cardsView cards isPlayable maybePlayedCard =
    let
        attrs c =
            let
                canBePlayed =
                    maybePlayedCard
                        |> Maybe.map (\playedCard -> canPlay playedCard c)
                        |> Maybe.withDefault True
            in
            if isPlayable then
                if canBePlayed then
                    [ onClick <| PlayedCard c, style "cursor" "pointer" ]

                else
                    [ style "cursor" "default", style "opacity" "0.5" ]

            else
                []
    in
    Html.ul []
        (cards
            |> List.map
                (\c ->
                    Html.li (attrs c) [ text <| cardToString c ]
                )
        )


playerHandsView : List Player -> List Card -> Html Msg
playerHandsView players discardStack =
    let
        maybePreviousCard =
            discardStack |> List.reverse >> List.head
    in
    case players of
        currentPlayer :: _ ->
            div []
                (players
                    |> List.map
                        (\p ->
                            div []
                                [ Html.h4 [] [ text <| p.name ++ "(" ++ String.fromInt (List.length p.hand) ++ ")" ]
                                , cardsView p.hand (p == currentPlayer) maybePreviousCard
                                ]
                        )
                )

        [] ->
            Html.text ""


playingView : Game -> Html Msg
playingView game =
    Html.div []
        [ Html.h3 [] [ Html.text "players" ]
        , playerHandsView game.players game.discardStack
        , Html.hr [] []
        , Html.h3 [ onClick TookFromDrawStack ] [ Html.text <| "drawStack " ++ "(" ++ String.fromInt (List.length game.drawStack) ++ ")" ]
        , Html.hr [] []
        , Html.h3 [] [ Html.text "discardStack" ]

        --, cardsView game.discardStack False Nothing
        --, Html.hr [][]
        , (game.discardStack |> List.head)
            |> Maybe.map (\c -> cardsView [ c ] False Nothing)
            |> Maybe.withDefault (Html.text "")
        ]


gameOver : Player -> Html Msg
gameOver player =
    Html.div []
        [ Html.h3 [] [ Html.text "GameOver" ]
        , Html.h4 [] [ Html.text (player.name ++ " is the winner !") ]
        , Html.button [ onClick RequestedStartGame ] [ text "Play again" ]
        ]


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            notStartedView

        Playing game ->
            playingView game

        GameOver player ->
            gameOver player


takeFromDrawStack : Game -> Game
takeFromDrawStack game =
    case game.players of
        currentPlayer :: _ ->
            let
                newHand =
                    game.drawStack
                        |> List.reverse
                        |> List.head
                        |> Maybe.map (\c -> currentPlayer.hand |> List.append [ c ])
                        |> Maybe.withDefault currentPlayer.hand

                newPlayers =
                    game.players
                        |> List.map
                            (\p ->
                                if p == currentPlayer then
                                    { p | hand = newHand }

                                else
                                    p
                            )
            in
            { game | players = newPlayers |> List.reverse, drawStack = game.drawStack |> List.reverse |> List.drop 1 }

        _ ->
            game


playCard : Card -> Game -> Game
playCard card game =
    case game.players of
        [ _ ] ->
            game

        currentPlayer :: _ ->
            let
                newHand =
                    currentPlayer.hand
                        |> List.filter
                            (\c ->
                                c /= card
                            )
            in
            { game
                | players =
                    game.players
                        |> List.map
                            (\p ->
                                if p == currentPlayer then
                                    { p | hand = newHand }

                                else
                                    p
                            )
                        |> List.reverse
                , discardStack = game.discardStack |> List.append [ card ]
            }

        _ ->
            game


minOfField : (a -> comparable) -> List a -> Maybe a
minOfField field =
    List.head << List.sortBy field


findWinner : List Player -> List Card -> Maybe Player
findWinner players drawStack =
    case drawStack of
        [] ->
            players
                |> List.map (\p -> { player = p, size = List.length p.hand })
                |> minOfField .size
                |> Maybe.map (\{ player } -> player)

        _ ->
            case players |> List.filter (\p -> List.isEmpty p.hand) of
                [] ->
                    Nothing

                [ p ] ->
                    Just p

                _ ->
                    Nothing


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( _, RequestedStartGame ) ->
            initialModel

        ( Playing game, TookFromDrawStack ) ->
            Playing (game |> takeFromDrawStack)

        ( Playing game, PlayedCard card ) ->
            let
                newGame =
                    playCard card game
            in
            case findWinner newGame.players game.drawStack of
                Nothing ->
                    Playing newGame

                Just player ->
                    GameOver player

        ( _, _ ) ->
            model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
