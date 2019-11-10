module Constants exposing (..)

import Types exposing (Card, Color(..), NeoPlayer, Player)


stackThickness : Int
stackThickness =
    6


neoPlayersInit : List NeoPlayer
neoPlayersInit =
    [ { id = 1
      , name = "Player 1"
      , avatar = card_noAvatar
      , avatarStyle = card_noAvatarStyle
      , isNeoPlayerChecked = False
      }
    , { id = 2
      , name = "Player 2"
      , avatar = card_noAvatar
      , avatarStyle = card_noAvatarStyle
      , isNeoPlayerChecked = False
      }
    , { id = 3
      , name = "Player 3"
      , avatar = card_noAvatar
      , avatarStyle = card_noAvatarStyle
      , isNeoPlayerChecked = False
      }
    ]


limitedDrawCards : Int
limitedDrawCards =
    5


nbBlacks : Int
nbBlacks =
    8


nbColors : Int
nbColors =
    4


nbCardsByColor : Int
nbCardsByColor =
    26



-- (0 à 9 + double + Sens + Pass)*2


nbAvatars : Int
nbAvatars =
    10


nbAvatarsStyles : Int
nbAvatarsStyles =
    10


nbPlayers : Int
nbPlayers =
    3


nbCardsByPlayer : Int
nbCardsByPlayer =
    7


noPlayer : Player
noPlayer =
    { id = 0
    , name = "NoBody"
    , hand = []
    , avatar = card_Back
    , avatarStyle = card_Back
    }


noNeoPlayer : NeoPlayer
noNeoPlayer =
    { id = 0
    , name = "NoBody"
    , avatar = card_Back
    , avatarStyle = card_Back
    , isNeoPlayerChecked = False
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


card_Drawed : Card
card_Drawed =
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


card_DrawImpossible : Card
card_DrawImpossible =
    { id = 0
    , value = 4
    , color = Black
    }


card_DrawGenerate : Card
card_DrawGenerate =
    { id = 0
    , value = 5
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


card_noAvatar : Card
card_noAvatar =
    { id = 0
    , value = 0
    , color = Red
    }


card_noAvatarStyle : Card
card_noAvatarStyle =
    { id = 0
    , value = 0
    , color = Black
    }


cZoomAction : Float
cZoomAction =
    1.0


cZoom : Float
cZoom =
    1.0


cZoomAvatar : Float
cZoomAvatar =
    1.2


cPlayableUp : Float
cPlayableUp =
    13


cSpriteSize : Float
cSpriteSize =
    1400


cRapportSprite : Float
cRapportSprite =
    3029 / 1454


cRapportCard : Float
cRapportCard =
    256 / 171


cIntersec : Float
cIntersec =
    (cSpriteSize * (3 - cRapportCard * cRapportSprite)) / (2 * cRapportSprite * (9 - 8 * cRapportCard))


cWidth : Float
cWidth =
    (cSpriteSize - 16 * cIntersec) / 15


cHeight : Float
cHeight =
    cRapportCard * cWidth


cStepX : Float
cStepX =
    cIntersec + cWidth


cStepY : Float
cStepY =
    cIntersec + cHeight


cOffsetX : Float
cOffsetX =
    -cIntersec


cOffsetY : Float
cOffsetY =
    -1


cMargin : Float



----- cacher 25% de la carte


cMargin =
    (-25 / 100) * cWidth
