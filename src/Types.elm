module Types exposing (..)

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

type StackShadow
    = Right 
    | Left

type Position
    = Absolute 
    | Relative 
    | Static

type alias Card =
    { id : Int
    , value : Int
    , color : Color
    }


type alias Player =
    { id : Int
    , name : String
    , hand : List Card
    , avatar : Card
    , avatarStyle : Card
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

type alias PlayerModel =
    {
        inputName : String,
        inputAvatar : Int,
        inputAvatarStyle : Int,
        players : List Player
    }



type Msg
    = RequestedStartGame
    | CardPlayed Card
    | DrawCard
    | DoNothing
    | GameEnded
    | GenerateDrawStack
    | Pass
    | DemandeNewListCards
    | DistributeDrawStack (List Int)
    | SetWildCardColor Color
    | RequestSetAvatar
    | SetPlayerName String
    | SetAvatar Int
    | SetAvatarStyle Int
    | ValiderAvatar Int


type Model
    = NotStarted
    | SettingAvatar PlayerModel
    | Playing Game
    | GameOver (List Player)
