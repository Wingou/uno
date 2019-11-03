
module Asset exposing (Image, src, unoSprite, path, pathFilename, imgStars)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES

imgStars : Image
imgStars =
    image "stars.png"

unoSprite : Image
unoSprite =
    image "unoSprite7.png"

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

