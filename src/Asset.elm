
module Asset exposing (ImageString, src, path, pathFilename, imgStars, spriteCards, spriteAvatars, panelAvatars, imgAvatarBorder1)

import Html exposing (Attribute)
import Html.Attributes as Attr


type ImageString
    = Image String



-- IMAGES

imgStars : ImageString
imgStars =
    image "stars.png"

panelCards : ImageString
panelCards =
    image "unoSprite7.png"

panelAvatars : ImageString
panelAvatars =
    image "avatarSprite1.png"

imgAvatarBorder1 : ImageString
imgAvatarBorder1 =
    image "avatarBorder1.png"

-------------------------------

path : String
path = "/assets/images/"

-------------------------------------------- 

spriteCards : String
spriteCards =
    pathFilename panelCards

spriteAvatars : String
spriteAvatars =
    pathFilename panelAvatars

--------------------------------------------

image : String -> ImageString
image name =
    Image (path ++ name)

src : ImageString -> Attribute msg
src (Image url) =
    Attr.src url

pathFilename : ImageString -> String
pathFilename (Image name) =
    name
