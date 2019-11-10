module Update exposing (..)

import Constants exposing (..)
import Functions exposing (..)
import List exposing (append, drop, head, map, range, reverse, take)
import Maybe exposing (withDefault)
import Random
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DemandeNewAvatarId, _ ) ->
            ( model, Random.generate DistributeAvatar randomAvatarId )

        ( DemandeNewAvatarStyleId, _ ) ->
            ( model, Random.generate DistributeAvatarStyle randomAvatarStyleId )

        ( DistributeAvatar randomAvatarId, SettingAvatar avatarModel ) ->
            ( SettingAvatar
                { avatarModel
                    | neoPlayers =
                        map
                            (\p ->
                                { p
                                    | avatar =
                                        if not p.isNeoPlayerChecked then
                                            getCard (withDefault 1 (head (drop (p.id - 1) randomAvatarId))) Red

                                        else
                                            p.avatar
                                }
                            )
                            avatarModel.neoPlayers
                }
            , Random.generate DistributeAvatarStyle randomAvatarStyleId
            )

        ( DistributeAvatarStyle randomAvatarStyleId, SettingAvatar avatarModel ) ->
            ( SettingAvatar
                { avatarModel
                    | neoPlayers =
                        map
                            (\p ->
                                { p
                                    | avatarStyle =
                                        if not p.isNeoPlayerChecked then
                                            getCard (withDefault 1 (head (drop (p.id - 1) randomAvatarStyleId))) Black

                                        else
                                            p.avatarStyle
                                    , isNeoPlayerChecked = True
                                }
                            )
                            avatarModel.neoPlayers
                }
            , Cmd.none
            )

        ( DemandeNewListCards, _ ) ->
            ( model, Random.generate DistributeDrawStack newIndicesGenerator )

        ( DistributeDrawStack generatedNewIds, Playing game ) ->
            let
                shuffleCards =
                    initShuffleCards drawStackInit generatedNewIds
            in
            ( Playing
                { game
                    | originStack = drawStackInit
                    , mainCard = firstCard shuffleCards
                    , players = initHandOfPlayers (drop 1 shuffleCards) game.players
                    , drawStack = take limitedDrawCards (drop (nbCardsByPlayer * nbPlayers + 1) shuffleCards)
                    , discardStack = firstCard shuffleCards :: []
                }
            , Cmd.none
            )

        ( RequestSetAvatar, _ ) ->
            ( SettingAvatar
                { inputName = ""
                , neoPlayers = neoPlayersInit
                , inputAvatar = 0
                , inputAvatarStyle = 0
                , avatarId = 0
                }
            , Cmd.none
            )

        ( RequestedStartGame, SettingAvatar avatarModel ) ->
            ( Playing
                { originStack = []
                , mainCard = card_Back
                , reverse = ToRight
                , players =
                    map
                        (\a ->
                            { avatar = a.avatar
                            , avatarStyle = a.avatarStyle
                            , hand = []
                            , id = a.id
                            , name = a.name
                            }
                        )
                        avatarModel.neoPlayers
                , drawStack = []
                , discardStack = []
                , drawing = 1
                , penality = False
                }
            , Random.generate DistributeDrawStack newIndicesGenerator
            )

        ( CardPlayed cardPlayed, Playing game ) ->
            let
                let_OmitPlayedCard =
                    omitPlayedCard cardPlayed game.players
            in
            if hasWinner game.players cardPlayed then
                ( GameOver let_OmitPlayedCard
                , Cmd.none
                )

            else
                case ( cardPlayed.value, cardPlayed.color ) of
                    ( 14, Black ) ->
                        ------- Wild Card
                        ( Playing
                            { game
                                | players =
                                    reverseDirection
                                        let_OmitPlayedCard
                                        ToStay
                                , discardStack = cardPlayed :: game.discardStack
                                , mainCard = cardPlayed
                                , penality = True
                            }
                        , Cmd.none
                        )

                    ( 13, Black ) ->
                        ----- Wild Draw 4 Card
                        ( Playing
                            { game
                                | players =
                                    reverseDirection
                                        let_OmitPlayedCard
                                        ToStay
                                , discardStack = cardPlayed :: game.discardStack
                                , drawing = getDrawing cardPlayed game.drawing
                                , mainCard = cardPlayed
                                , penality = True
                            }
                        , Cmd.none
                        )

                    ( _, _ ) ->
                        let
                            let_Reverse =
                                getReverse cardPlayed.value game.reverse
                        in
                        ( Playing
                            { game
                                | reverse = let_Reverse
                                , players =
                                    reverseDirection
                                        let_OmitPlayedCard
                                        let_Reverse
                                , discardStack = cardPlayed :: game.discardStack
                                , drawing = getDrawing cardPlayed game.drawing
                                , mainCard = cardPlayed
                                , penality =
                                    (cardPlayed.value == 12 && cardPlayed.color /= Black)
                                        || (cardPlayed.value == 13 && cardPlayed.color == Black)
                                        || (cardPlayed.value == 10 && cardPlayed.color /= Black)
                            }
                        , Cmd.none
                        )

        ( DrawCard, Playing game ) ->
            ( Playing
                { game
                    | players = drawCardToPlayer game.players game.drawStack game.drawing
                    , drawStack = drop game.drawing game.drawStack
                    , drawing = 0
                }
            , Cmd.none
            )

        ( GenerateDrawStack, Playing game ) ->
            let
                makeWildCard =
                    map
                        (\c ->
                            { c
                                | color =
                                    if c.value == 13 || c.value == 14 then
                                        Black

                                    else
                                        c.color
                            }
                        )
                        (tailCard game.discardStack)
            in
            ( Playing
                { game
                    | drawStack = append game.drawStack makeWildCard
                    , discardStack = firstCard game.discardStack :: []
                }
            , Cmd.none
            )

        ( Pass, Playing game ) ->
            ( Playing
                { game
                    | players = reverseDirection game.players game.reverse
                    , drawing = 1
                    , penality = False
                }
            , Cmd.none
            )

        ( SetWildCardColor color, Playing game ) ->
            let
                coloredWildCard =
                    game.mainCard
            in
            ( Playing
                { game
                    | players =
                        reverseDirection
                            game.players
                            game.reverse
                    , discardStack = { coloredWildCard | color = color } :: tailCard game.discardStack
                    , mainCard = { coloredWildCard | color = color }
                    , penality = coloredWildCard.value == 13
                }
            , Cmd.none
            )

        ( SetPlayerName pseudo, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel
                    | inputName = pseudo
                }
            , Cmd.none
            )

        ( SetAvatar value, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel
                    | inputAvatar = value
                }
            , Cmd.none
            )

        ( SetAvatarStyle value, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel
                    | inputAvatarStyle = value
                }
            , Cmd.none
            )

        ( ValiderAvatar id, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel
                    | neoPlayers =
                        map
                            (\p ->
                                if p.id == id then
                                    { p
                                        | avatar = getCard playerModel.inputAvatar Red
                                        , avatarStyle = getCard playerModel.inputAvatarStyle Black
                                        , name = playerModel.inputName
                                        , isNeoPlayerChecked = True
                                    }

                                else
                                    p
                            )
                            playerModel.neoPlayers
                    , avatarId = 0
                    , inputAvatar = 0
                    , inputAvatarStyle = 0
                    , inputName = ""
                }
            , Cmd.none
            )

        ( SetAvataring neoPlayer, SettingAvatar playerModel ) ->
            let
                avatarId =
                    neoPlayer.id

                inputAvatar =
                    neoPlayer.avatar.value

                inputAvatarStyle =
                    neoPlayer.avatarStyle.value

                inputName =
                    neoPlayer.name
            in
            ( SettingAvatar
                { playerModel
                    | avatarId = avatarId
                    , inputAvatar = inputAvatar
                    , inputAvatarStyle = inputAvatarStyle
                    , inputName = inputName
                    , neoPlayers =
                        map
                            (\p ->
                                { p
                                    | isNeoPlayerChecked =
                                        if avatarId == p.id then
                                            False

                                        else
                                            p.isNeoPlayerChecked
                                }
                            )
                            playerModel.neoPlayers
                }
            , Cmd.none
            )

        ( RandomizeAvatar, SettingAvatar playerModel ) ->
            ( SettingAvatar playerModel, Random.generate DistributeAvatar randomAvatarId )

        ( DoNothing, _ ) ->
            ( model, Cmd.none )

        ( GameEnded, Playing game ) ->
            ( GameOver game.players, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )
