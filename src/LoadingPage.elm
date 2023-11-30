module LoadingPage exposing
    ( cursorActualPosition
    , cursorPosition
    , devicePixelRatioChanged
    , handleOutMsg
    , hoverAt
    , loadingCanvasView
    , mouseListeners
    , mouseScreenPosition
    , mouseWorldPosition
    , shortDelayDuration
    , update
    , updateLocalModel
    , windowResizedUpdate
    )

import Array
import AssocList
import Audio exposing (AudioCmd)
import Bounds exposing (Bounds)
import Change exposing (BackendReport, Change(..), Report, UserStatus(..))
import Color exposing (Colors)
import Coord exposing (Coord)
import Cursor exposing (Cursor)
import Duration exposing (Duration)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Task
import Effect.Time as Time
import Effect.WebGL
import Effect.WebGL.Texture exposing (Texture)
import Html exposing (Html)
import Html.Attributes
import Html.Events.Extra.Mouse exposing (Button(..))
import Id exposing (AnimalId, Id, TrainId, UserId)
import IdDict exposing (IdDict)
import Image
import Keyboard
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import LocalGrid exposing (LocalGrid, LocalGrid_)
import Math.Matrix4 as Mat4
import Math.Vector4 as Vec4
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Ports
import Quantity exposing (Quantity)
import Shaders
import Sound
import Sprite exposing (Vertex)
import Terrain
import Tool exposing (Tool(..))
import Toolbar
import Types exposing (CssPixels, FrontendLoaded, FrontendLoading, FrontendModel_(..), FrontendMsg_(..), Hover(..), LoadedLocalModel_, LoadingLocalModel(..), MouseButtonState(..), SubmitStatus(..), ToBackend(..), ToolButton(..), UiHover(..), ViewPoint(..))
import Ui
import Units exposing (CellUnit, WorldUnit)
import WebGL.Texture


update :
    FrontendMsg_
    -> FrontendLoading
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update msg loadingModel =
    case msg of
        WindowResized windowSize ->
            windowResizedUpdate windowSize loadingModel |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

        GotDevicePixelRatio devicePixelRatio ->
            devicePixelRatioChanged devicePixelRatio loadingModel
                |> (\( a, b ) -> ( Loading a, b, Audio.cmdNone ))

        SoundLoaded sound result ->
            ( Loading { loadingModel | sounds = AssocList.insert sound result loadingModel.sounds }
            , Command.none
            , Audio.cmdNone
            )

        TextureLoaded result ->
            case result of
                Ok texture ->
                    ( Loading { loadingModel | texture = Just texture }, Command.none, Sound.load SoundLoaded )

                Err _ ->
                    ( Loading loadingModel, Command.none, Audio.cmdNone )

        LightsTextureLoaded result ->
            case result of
                Ok texture ->
                    ( Loading { loadingModel | lightsTexture = Just texture }, Command.none, Audio.cmdNone )

                Err _ ->
                    ( Loading loadingModel, Command.none, Audio.cmdNone )

        DepthTextureLoaded result ->
            case result of
                Ok texture ->
                    ( Loading { loadingModel | depthTexture = Just texture }, Command.none, Audio.cmdNone )

                Err _ ->
                    ( Loading loadingModel, Command.none, Audio.cmdNone )

        MouseMove mousePosition ->
            ( Loading { loadingModel | mousePosition = mousePosition }, Command.none, Audio.cmdNone )

        MouseUp MainButton mousePosition ->
            if insideStartButton mousePosition loadingModel then
                case tryLoading loadingModel of
                    Just a ->
                        a ()

                    Nothing ->
                        ( Loading loadingModel, Command.none, Audio.cmdNone )

            else
                ( Loading loadingModel, Command.none, Audio.cmdNone )

        KeyDown rawKey ->
            case Keyboard.anyKeyOriginal rawKey of
                Just Keyboard.Enter ->
                    case tryLoading loadingModel of
                        Just a ->
                            a ()

                        Nothing ->
                            ( Loading loadingModel, Command.none, Audio.cmdNone )

                _ ->
                    ( Loading loadingModel, Command.none, Audio.cmdNone )

        AnimationFrame time ->
            ( Loading { loadingModel | time = Just time }, Command.none, Audio.cmdNone )

        _ ->
            ( Loading loadingModel, Command.none, Audio.cmdNone )


tryLoading :
    FrontendLoading
    ->
        Maybe
            (()
             ->
                ( FrontendModel_
                , Command FrontendOnly ToBackend FrontendMsg_
                , AudioCmd FrontendMsg_
                )
            )
tryLoading frontendLoading =
    case frontendLoading.localModel of
        LoadingLocalModel _ ->
            Nothing

        LoadedLocalModel loadedLocalModel ->
            Maybe.map4
                (\time texture lightsTexture depthTexture () ->
                    loadedInit time frontendLoading texture lightsTexture depthTexture loadedLocalModel
                )
                frontendLoading.time
                frontendLoading.texture
                frontendLoading.lightsTexture
                frontendLoading.depthTexture


loadedInit :
    Time.Posix
    -> FrontendLoading
    -> Texture
    -> Texture
    -> Texture
    -> LoadedLocalModel_
    -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
loadedInit time loading texture lightsTexture depthTexture loadedLocalModel =
    let
        viewpoint =
            Coord.toPoint2d loading.viewPoint |> NormalViewPoint

        mouseLeft =
            MouseButtonUp { current = loading.mousePosition }

        mouseMiddle =
            MouseButtonUp { current = loading.mousePosition }

        model : FrontendLoaded
        model =
            { key = loading.key
            , localModel = loadedLocalModel.localModel
            , viewPoint = viewpoint
            , texture = texture
            , lightsTexture = lightsTexture
            , depthTexture = depthTexture
            , trainTexture = Nothing
            , trainLightsTexture = Nothing
            , trainDepthTexture = Nothing
            , pressedKeys = []
            , windowSize = loading.windowSize
            , cssWindowSize = loading.cssWindowSize
            , cssCanvasSize = loading.cssCanvasSize
            , devicePixelRatio = loading.devicePixelRatio
            , zoomFactor = loading.zoomFactor
            , mouseLeft = mouseLeft
            , mouseMiddle = mouseMiddle
            , pendingChanges = []
            , time = time
            , startTime = time
            , animationElapsedTime = Duration.seconds 0
            , sounds = loading.sounds
            , musicVolume = loading.musicVolume
            , soundEffectVolume = loading.soundEffectVolume
            , ui = Ui.none
            , uiMesh = Shaders.triangleFan []
            , eventIdCounter = Id.fromInt 0
            , pingData = Nothing
            , pingStartTime = Just time
            , localTime = time
            , focus = Nothing
            , previousFocus = Nothing
            , topMenuOpened = Nothing
            , isReconnecting = False
            , lastCheckConnection = time
            , hideUi = False
            }
    in
    ( model
    , Command.batch
        [ Effect.WebGL.Texture.loadWith
            { magnify = Effect.WebGL.Texture.nearest
            , minify = Effect.WebGL.Texture.nearest
            , horizontalWrap = Effect.WebGL.Texture.clampToEdge
            , verticalWrap = Effect.WebGL.Texture.clampToEdge
            , flipY = False
            }
            "/trains.png"
            |> Effect.Task.attempt TrainTextureLoaded
        , Effect.WebGL.Texture.loadWith
            { magnify = Effect.WebGL.Texture.nearest
            , minify = Effect.WebGL.Texture.nearest
            , horizontalWrap = Effect.WebGL.Texture.clampToEdge
            , verticalWrap = Effect.WebGL.Texture.clampToEdge
            , flipY = False
            }
            "/train-lights.png"
            |> Effect.Task.attempt TrainLightsTextureLoaded
        , Effect.WebGL.Texture.loadWith
            { magnify = Effect.WebGL.Texture.nearest
            , minify = Effect.WebGL.Texture.nearest
            , horizontalWrap = Effect.WebGL.Texture.clampToEdge
            , verticalWrap = Effect.WebGL.Texture.clampToEdge
            , flipY = False
            }
            "/train-depth.png"
            |> Effect.Task.attempt TrainDepthTextureLoaded
        , Effect.Lamdera.sendToBackend PingRequest
        ]
    )
        |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))


mouseWorldPosition :
    { a
        | mouseLeft : MouseButtonState
        , windowSize : ( Quantity Int Pixels, Quantity Int Pixels )
        , devicePixelRatio : Float
        , zoomFactor : Int
        , mouseMiddle : MouseButtonState
        , viewPoint : ViewPoint
        , time : Time.Posix
    }
    -> Point2d WorldUnit WorldUnit
mouseWorldPosition model =
    mouseScreenPosition model |> Toolbar.screenToWorld model


mouseScreenPosition : { a | mouseLeft : MouseButtonState } -> Point2d Pixels Pixels
mouseScreenPosition model =
    case model.mouseLeft of
        MouseButtonDown { current } ->
            current

        MouseButtonUp { current } ->
            current


cursorPosition :
    { a | size : Coord WorldUnit }
    ->
        { b
            | mouseLeft : MouseButtonState
            , windowSize : ( Quantity Int Pixels, Quantity Int Pixels )
            , devicePixelRatio : Float
            , zoomFactor : Int
            , mouseMiddle : MouseButtonState
            , viewPoint : ViewPoint
            , time : Time.Posix
        }
    -> Coord WorldUnit
cursorPosition tileData model =
    mouseWorldPosition model
        |> Coord.floorPoint
        |> Coord.minus (tileData.size |> Coord.divide (Coord.tuple ( 2, 2 )))


updateLocalModel : Change.LocalChange -> FrontendLoaded -> ( FrontendLoaded, LocalGrid.OutMsg )
updateLocalModel msg model =
    let
        ( newLocalModel, outMsg ) =
            LocalGrid.update (LocalChange model.eventIdCounter msg) model.localModel
    in
    ( { model
        | pendingChanges = ( model.eventIdCounter, msg ) :: model.pendingChanges
        , localModel = newLocalModel
        , eventIdCounter = Id.increment model.eventIdCounter
      }
    , outMsg
    )


hoverAt : FrontendLoaded -> Point2d Pixels Pixels -> Hover
hoverAt model mousePosition =
    let
        mousePosition2 : Coord Pixels
        mousePosition2 =
            mousePosition
                |> Coord.roundPoint
    in
    case Ui.hover mousePosition2 model.ui of
        Ui.InputHover data ->
            UiHover data.id { position = data.position }

        Ui.BackgroundHover ->
            UiBackgroundHover

        Ui.NoHover ->
            MapHover


cursorActualPosition : Bool -> Id UserId -> Cursor -> FrontendLoaded -> Point2d WorldUnit WorldUnit
cursorActualPosition isCurrentUser userId cursor model =
    cursor.position


shortDelayDuration : Duration
shortDelayDuration =
    Duration.milliseconds 100


handleOutMsg :
    Bool
    -> ( FrontendLoaded, LocalGrid.OutMsg )
    -> ( FrontendLoaded, Command FrontendOnly toMsg FrontendMsg_ )
handleOutMsg isFromBackend ( model, outMsg ) =
    case outMsg of
        LocalGrid.NoOutMsg ->
            ( model, Command.none )


windowResizedUpdate :
    Coord CssPixels
    -> { b | cssWindowSize : Coord CssPixels, windowSize : Coord Pixels, cssCanvasSize : Coord CssPixels, devicePixelRatio : Float }
    ->
        ( { b | cssWindowSize : Coord CssPixels, windowSize : Coord Pixels, cssCanvasSize : Coord CssPixels, devicePixelRatio : Float }
        , Command FrontendOnly ToBackend msg
        )
windowResizedUpdate cssWindowSize model =
    let
        { cssCanvasSize, windowSize } =
            findPixelPerfectSize { devicePixelRatio = model.devicePixelRatio, cssWindowSize = cssWindowSize }
    in
    ( { model | cssWindowSize = cssWindowSize, cssCanvasSize = cssCanvasSize, windowSize = windowSize }
    , Ports.getDevicePixelRatio
    )


devicePixelRatioChanged :
    Float
    -> { a | cssWindowSize : Coord CssPixels, devicePixelRatio : Float, cssCanvasSize : Coord CssPixels, windowSize : Coord Pixels }
    -> ( { a | cssWindowSize : Coord CssPixels, devicePixelRatio : Float, cssCanvasSize : Coord CssPixels, windowSize : Coord Pixels }, Command restriction toMsg msg )
devicePixelRatioChanged devicePixelRatio model =
    let
        { cssCanvasSize, windowSize } =
            findPixelPerfectSize { devicePixelRatio = devicePixelRatio, cssWindowSize = model.cssWindowSize }
    in
    ( { model | devicePixelRatio = devicePixelRatio, cssCanvasSize = cssCanvasSize, windowSize = windowSize }
    , Command.none
    )


findPixelPerfectSize :
    { devicePixelRatio : Float, cssWindowSize : Coord CssPixels }
    -> { cssCanvasSize : Coord CssPixels, windowSize : Coord Pixels }
findPixelPerfectSize frontendModel =
    let
        findValue : Quantity Int CssPixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Quantity.unwrap value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * frontendModel.devicePixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * frontendModel.devicePixelRatio |> round ))
                |> Maybe.withDefault ( Quantity.unwrap value, toFloat (Quantity.unwrap value) * frontendModel.devicePixelRatio |> round )

        ( w, actualW ) =
            findValue (Tuple.first frontendModel.cssWindowSize)

        ( h, actualH ) =
            findValue (Tuple.second frontendModel.cssWindowSize)
    in
    { cssCanvasSize = Coord.xy w h, windowSize = Coord.xy actualW actualH }


loadingCanvasView : FrontendLoading -> Html FrontendMsg_
loadingCanvasView model =
    let
        ( windowWidth, windowHeight ) =
            Coord.toTuple model.windowSize

        ( cssWindowWidth, cssWindowHeight ) =
            Coord.toTuple model.cssCanvasSize

        loadingTextPosition2 =
            loadingTextPosition model.windowSize

        isHovering =
            insideStartButton model.mousePosition model

        showMousePointer =
            isHovering
    in
    Effect.WebGL.toHtmlWith
        [ Effect.WebGL.alpha False
        , Effect.WebGL.clearColor 1 1 1 1
        , Effect.WebGL.depth 1
        ]
        ([ Html.Attributes.width windowWidth
         , Html.Attributes.height windowHeight
         , Html.Attributes.style "cursor"
            (if showMousePointer then
                "pointer"

             else
                "default"
            )
         , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
         , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
         ]
            ++ mouseListeners model
        )
        (case
            ( Maybe.andThen Effect.WebGL.Texture.unwrap model.texture
            , Maybe.andThen Effect.WebGL.Texture.unwrap model.lightsTexture
            , Maybe.andThen Effect.WebGL.Texture.unwrap model.depthTexture
            )
         of
            ( Just texture, Just lightsTexture, Just depth ) ->
                let
                    textureSize =
                        WebGL.Texture.size texture |> Coord.tuple |> Coord.toVec2
                in
                Effect.WebGL.entityWith
                    [ Shaders.blend ]
                    Shaders.vertexShader
                    Shaders.fragmentShader
                    touchDevicesNotSupportedMesh
                    { view =
                        Mat4.makeScale3
                            (2 / toFloat windowWidth)
                            (-2 / toFloat windowHeight)
                            1
                            |> Coord.translateMat4 (Coord.tuple ( -windowWidth // 2, -windowHeight // 2 ))
                            |> Coord.translateMat4 (touchDevicesNotSupportedPosition model.windowSize)
                    , texture = texture
                    , lights = lightsTexture
                    , depth = depth
                    , textureSize = textureSize
                    , color = Vec4.vec4 1 1 1 1
                    , userId = Shaders.noUserIdSelected
                    , time = 0
                    , night = 0
                    , waterReflection = 0
                    }
                    :: (case tryLoading model of
                            Just _ ->
                                [ Effect.WebGL.entityWith
                                    [ Shaders.blend ]
                                    Shaders.vertexShader
                                    Shaders.fragmentShader
                                    (if isHovering then
                                        startButtonHighlightMesh

                                     else
                                        startButtonMesh
                                    )
                                    { view =
                                        Mat4.makeScale3
                                            (2 / toFloat windowWidth)
                                            (-2 / toFloat windowHeight)
                                            1
                                            |> Coord.translateMat4 (Coord.tuple ( -windowWidth // 2, -windowHeight // 2 ))
                                            |> Coord.translateMat4 loadingTextPosition2
                                    , texture = texture
                                    , lights = lightsTexture
                                    , depth = depth
                                    , textureSize = textureSize
                                    , color = Vec4.vec4 1 1 1 1
                                    , userId = Shaders.noUserIdSelected
                                    , time = 0
                                    , night = 0
                                    , waterReflection = 0
                                    }
                                ]

                            Nothing ->
                                [ Effect.WebGL.entityWith
                                    [ Shaders.blend ]
                                    Shaders.vertexShader
                                    Shaders.fragmentShader
                                    loadingTextMesh
                                    { view =
                                        Mat4.makeScale3
                                            (2 / toFloat windowWidth)
                                            (-2 / toFloat windowHeight)
                                            1
                                            |> Coord.translateMat4
                                                (Coord.tuple ( -windowWidth // 2, -windowHeight // 2 ))
                                            |> Coord.translateMat4 (loadingTextPosition model.windowSize)
                                    , texture = texture
                                    , lights = lightsTexture
                                    , depth = depth
                                    , textureSize = textureSize
                                    , color = Vec4.vec4 1 1 1 1
                                    , userId = Shaders.noUserIdSelected
                                    , time = 0
                                    , night = 0
                                    , waterReflection = 0
                                    }
                                ]
                       )

            _ ->
                []
        )


mouseListeners : { a | devicePixelRatio : Float } -> List (Html.Attribute FrontendMsg_)
mouseListeners model =
    [ Html.Events.Extra.Mouse.onDown
        (\{ clientPos, button } ->
            MouseDown
                button
                (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos)
                    |> Point2d.scaleAbout Point2d.origin model.devicePixelRatio
                )
        )
    , Html.Events.Extra.Mouse.onMove
        (\{ clientPos } ->
            MouseMove
                (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos)
                    |> Point2d.scaleAbout Point2d.origin model.devicePixelRatio
                )
        )
    , Html.Events.Extra.Mouse.onUp
        (\{ clientPos, button } ->
            MouseUp
                button
                (Point2d.pixels (Tuple.first clientPos) (Tuple.second clientPos)
                    |> Point2d.scaleAbout Point2d.origin model.devicePixelRatio
                )
        )
    , Html.Events.Extra.Mouse.onContextMenu (\_ -> NoOpFrontendMsg)
    ]


insideStartButton : Point2d Pixels Pixels -> { a | devicePixelRatio : Float, windowSize : Coord Pixels } -> Bool
insideStartButton mousePosition model =
    let
        mousePosition2 : Coord Pixels
        mousePosition2 =
            mousePosition
                |> Coord.roundPoint

        loadingTextPosition2 =
            loadingTextPosition model.windowSize
    in
    Bounds.fromCoordAndSize loadingTextPosition2 loadingTextSize |> Bounds.contains mousePosition2


loadingTextPosition : Coord units -> Coord units
loadingTextPosition windowSize =
    windowSize
        |> Coord.divide (Coord.xy 2 2)
        |> Coord.minus (Coord.divide (Coord.xy 2 2) loadingTextSize)


loadingTextSize : Coord units
loadingTextSize =
    Coord.xy 336 54


loadingTextMesh : Effect.WebGL.Mesh Vertex
loadingTextMesh =
    Sprite.text Color.black 2 "Loading..." Coord.origin
        |> Sprite.toMesh


touchDevicesNotSupportedPosition : Coord units -> Coord units
touchDevicesNotSupportedPosition windowSize =
    loadingTextPosition windowSize |> Coord.plus (Coord.yOnly loadingTextSize |> Coord.multiply (Coord.xy 1 2))


touchDevicesNotSupportedMesh : Effect.WebGL.Mesh Vertex
touchDevicesNotSupportedMesh =
    Sprite.text Color.black 2 "(Phones and tablets not supported)" (Coord.xy -170 0)
        |> Sprite.toMesh


startButtonMesh : Effect.WebGL.Mesh Vertex
startButtonMesh =
    Sprite.spriteWithColor
        (Color.rgb255 157 143 134)
        Coord.origin
        loadingTextSize
        (Coord.xy 508 28)
        (Coord.xy 1 1)
        ++ Sprite.sprite
            (Coord.xy 2 2)
            (loadingTextSize |> Coord.minus (Coord.xy 4 4))
            (Coord.xy 507 28)
            (Coord.xy 1 1)
        ++ Sprite.text Color.black 2 "Press to start!" (Coord.xy 16 8)
        |> Sprite.toMesh


startButtonHighlightMesh : Effect.WebGL.Mesh Vertex
startButtonHighlightMesh =
    Sprite.spriteWithColor
        (Color.rgb255 241 231 223)
        Coord.origin
        loadingTextSize
        (Coord.xy 508 28)
        (Coord.xy 1 1)
        ++ Sprite.sprite
            (Coord.xy 2 2)
            (loadingTextSize |> Coord.minus (Coord.xy 4 4))
            (Coord.xy 505 28)
            (Coord.xy 1 1)
        ++ Sprite.text Color.black 2 "Press to start!" (Coord.xy 16 8)
        |> Sprite.toMesh
