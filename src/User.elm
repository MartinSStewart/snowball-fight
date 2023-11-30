module User exposing (FrontendUser, InviteTree(..))

import Color exposing (Color, Colors)
import Coord exposing (Coord)
import Cursor exposing (Cursor)
import DisplayName exposing (DisplayName)
import Id exposing (Id, UserId)
import IdDict exposing (IdDict)
import List.Extra
import Pixels exposing (Pixels)
import Sprite
import Ui exposing (BorderAndFill(..))


type alias FrontendUser =
    {}


type InviteTree
    = InviteTree
        { userId : Id UserId
        , invited : List InviteTree
        }


charScale : number
charScale =
    2


onlineColor : Color
onlineColor =
    Color.rgb255 80 255 100


dotSize : Coord Pixels
dotSize =
    Coord.xy 8 8


onlineIcon : Ui.Element id
onlineIcon =
    Ui.quads
        { size = Coord.scalar charScale Sprite.charSize
        , vertices =
            Sprite.rectangle onlineColor
                (Coord.scalar charScale Sprite.charSize |> Coord.minus dotSize |> Coord.divide (Coord.xy 2 2))
                dotSize
        }
