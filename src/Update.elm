module Update exposing (..)


import Types exposing (..)

import Functions exposing (..)
import List exposing (map, reverse, take, range, drop, append )
import Random
import Constants exposing (..)

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
                , drawStack = take limitedDrawCards (drop (nbCardsByPlayer * nbPlayers + 1) shuffleCards)
                , discardStack = firstCard shuffleCards :: []
                }
                , Cmd.none)

        ( RequestSetAvatar, _ ) ->
            let 
                avatar = avatarToBeSet playersInit
            in
            (SettingAvatar 
                {
                    inputName = "noname",
                    players = playersInit,
                    inputAvatar = 0,
                    inputAvatarStyle = 0
                }
                , Cmd.none
            )
     
        ( RequestedStartGame, SettingAvatar avatarModel ) ->
            (Playing
                {
                  originStack = []
                , mainCard = card_Back
                , reverse = ToRight  
                , players = avatarModel.players
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
                case (cardPlayed.value, cardPlayed.color)  of
                    (14, Black) -> ------- Wild Card
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

                    (13, Black) -> ----- Wild Draw 4 Card
                        (Playing
                            { game |
                                players = 
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

                    (_,_) ->
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
                                , drawing = getDrawing cardPlayed game.drawing
                                , mainCard = cardPlayed
                                , penality = (
                                                (cardPlayed.value==12 && cardPlayed.color/=Black)
                                             || (cardPlayed.value==13 && cardPlayed.color==Black)
                                             || (cardPlayed.value==10 && cardPlayed.color/=Black)
                                )
                            }
                        , Cmd.none    
                        )
        
        ( DrawCard, Playing game ) ->
            (Playing
                { game
                    | players = drawCardToPlayer game.players game.drawStack game.drawing
                    , drawStack = drop game.drawing game.drawStack
                    , drawing = 0
                }
             , Cmd.none    
            )

        ( GenerateDrawStack, Playing game ) ->
            let
                makeWildCard = map (\c -> { c | color=if c.value==13 || c.value==14 then
                                                        Black
                                                      else
                                                        c.color
                                            }
                                    ) (tailCard game.discardStack)
            in
            
            (Playing
                { game
                    | drawStack = append game.drawStack makeWildCard
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
                    , mainCard = { coloredWildCard | color=color}
                    , penality = coloredWildCard.value==13
                }
            , Cmd.none    
            )

        ( SetPlayerName pseudo, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel |
                    inputName = pseudo
                }
            , Cmd.none
            )

        ( SetAvatar value, SettingAvatar playerModel )  ->
            ( SettingAvatar
                { playerModel |
                    inputAvatar = value
                }
            , Cmd.none
            )

        ( SetAvatarStyle value, SettingAvatar playerModel )  ->
            ( SettingAvatar
                { playerModel |
                    inputAvatarStyle = value
                }
            , Cmd.none
            )

        (ValiderAvatar id, SettingAvatar playerModel ) ->
            ( SettingAvatar
                { playerModel | players = map (\p -> if p.id==id then
                                                    { 
                                                                id = id,
                                                                avatar = getCard playerModel.inputAvatar Red,
                                                                avatarStyle = getCard playerModel.inputAvatarStyle Black,
                                                                hand = [],
                                                                name = playerModel.inputName
                                                    }
                                                else
                                                    p
                                    ) playerModel.players
                                , inputAvatar = 0
                                , inputAvatarStyle = 0
                                , inputName = ""

                                 }
            , Cmd.none
            )

        ( DoNothing, _ ) ->
            (model, Cmd.none)

        ( GameEnded, Playing game ) ->
            (GameOver game.players, Cmd.none)



        ( _, _ ) ->
            (model, Cmd.none)

