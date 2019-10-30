
module Asset exposing (Image, src, unoSprite, arrowDown, arrowUp, path, pathFilename)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES


unoSprite : Image
unoSprite =
    image "unoSprite.png"


arrowUp : Image
arrowUp =
    image "up.png"

    
arrowDown : Image
arrowDown =
    image "down.png"


path : String
path = "/assets/images/"

image : String -> Image
image name =
    Image (path ++ name)



-- USING IMAGES


src : Image -> Attribute msg
src (Image url) =
    Attr.src url

pathFilename : Image -> String
pathFilename (Image name) =
    name

