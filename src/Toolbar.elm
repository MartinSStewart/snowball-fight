module Toolbar exposing (actualViewPoint, screenToWorld, view)

import Coord exposing (Coord)
import Effect.Time
import LocalGrid
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..), Rate)
import Types exposing (ContextMenu, FrontendLoaded, Hover(..), MouseButtonState(..), SubmitStatus(..), ToolButton(..), TopMenu(..), UiHover(..), ViewPoint(..))
import Ui exposing (BorderAndFill(..))
import Units exposing (WorldUnit)
import Vector2d


view : FrontendLoaded -> Hover -> Ui.Element UiHover
view model hover =
    let
        localModel : LocalGrid.LocalGrid_
        localModel =
            LocalGrid.localModel model.localModel

        ( cssWindowWidth, cssWindowHeight ) =
            Coord.toTuple model.cssWindowSize

        windowSize =
            Coord.xy
                (round (toFloat cssWindowWidth * model.devicePixelRatio))
                (round (toFloat cssWindowHeight * model.devicePixelRatio))
    in
    normalView windowSize model hover


normalView : Coord Pixels -> FrontendLoaded -> Hover -> Ui.Element UiHover
normalView windowSize model hover =
    Ui.bottomCenter
        { size = windowSize
        , inFront =
            if model.hideUi then
                []

            else
                []
        }
        Ui.none


screenToWorld :
    { a
        | windowSize : ( Quantity Int sourceUnits, Quantity Int sourceUnits )
        , devicePixelRatio : Float
        , zoomFactor : Int
        , mouseLeft : MouseButtonState
        , mouseMiddle : MouseButtonState
        , viewPoint : ViewPoint
        , time : Effect.Time.Posix
    }
    -> Point2d sourceUnits Pixels
    -> Point2d WorldUnit WorldUnit
screenToWorld model =
    let
        ( w, h ) =
            model.windowSize
    in
    Point2d.translateBy
        (Vector2d.xy (Quantity.toFloatQuantity w) (Quantity.toFloatQuantity h) |> Vector2d.scaleBy -0.5)
        >> point2dAt2 (scaleForScreenToWorld model)
        >> Point2d.placeIn (Units.screenFrame (actualViewPoint model))


scaleForScreenToWorld : { a | devicePixelRatio : Float, zoomFactor : Int } -> ( Quantity Float units, Quantity Float units )
scaleForScreenToWorld model =
    ( 1 / (toFloat model.zoomFactor * toFloat Units.tileWidth) |> Quantity
    , 1 / (toFloat model.zoomFactor * toFloat Units.tileHeight) |> Quantity
    )


actualViewPoint model =
    Point2d.origin


point2dAt2 :
    ( Quantity Float (Rate sourceUnits destinationUnits)
    , Quantity Float (Rate sourceUnits destinationUnits)
    )
    -> Point2d sourceUnits coordinates
    -> Point2d destinationUnits coordinates
point2dAt2 ( Quantity rateX, Quantity rateY ) point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    { x = x * rateX
    , y = y * rateY
    }
        |> Point2d.unsafe


point2dAt2_ :
    ( Quantity Float (Rate sourceUnits destinationUnits)
    , Quantity Float (Rate sourceUnits destinationUnits)
    )
    -> Point2d sourceUnits coordinates
    -> Point2d destinationUnits coordinates
point2dAt2_ ( Quantity rateX, Quantity rateY ) point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    { x = x / rateX
    , y = y / rateY
    }
        |> Point2d.unsafe
