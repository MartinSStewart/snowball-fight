module Frontend exposing (app, app_)

import AssocList
import Audio exposing (Audio, AudioCmd, AudioData)
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Change
import Color exposing (Color, Colors)
import Coord exposing (Coord)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Browser.Dom
import Effect.Browser.Events
import Effect.Browser.Navigation
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Effect.WebGL exposing (Mesh)
import Effect.WebGL.Texture
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse exposing (Button(..))
import IdDict
import Json.Decode
import Keyboard
import Lamdera
import List.Nonempty exposing (Nonempty(..))
import LoadingPage
import LocalGrid exposing (LocalGrid_)
import LocalModel
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector4
import PingData
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Ports
import Quantity exposing (Quantity(..))
import Random
import Shaders exposing (RenderData)
import Sprite exposing (Vertex)
import TextInput exposing (OutMsg(..))
import TextInputMultiline
import Tile exposing (Category(..), Tile(..), TileGroup(..))
import Time
import Toolbar
import Types exposing (ContextMenu, FrontendLoaded, FrontendModel_(..), FrontendMsg_(..), Hover(..), LoadingLocalModel(..), MouseButtonState(..), RemovedTileParticle, SubmitStatus(..), ToBackend(..), ToFrontend(..), ToolButton(..), TopMenu(..), UiHover(..), ViewPoint(..))
import Ui exposing (UiEvent)
import Units exposing (WorldUnit)
import Url exposing (Url)
import Vector2d exposing (Vector2d)
import WebGL.Texture


app :
    { init : Url -> Lamdera.Key -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , view : Audio.Model FrontendMsg_ FrontendModel_ -> Browser.Document (Audio.Msg FrontendMsg_)
    , update :
        Audio.Msg FrontendMsg_
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , updateFromBackend :
        ToFrontend
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Cmd (Audio.Msg FrontendMsg_) )
    , subscriptions : Audio.Model FrontendMsg_ FrontendModel_ -> Sub (Audio.Msg FrontendMsg_)
    , onUrlRequest : Browser.UrlRequest -> Audio.Msg FrontendMsg_
    , onUrlChange : Url -> Audio.Msg FrontendMsg_
    }
app =
    Effect.Lamdera.frontend Lamdera.sendToBackend app_


app_ :
    { init :
        Url
        -> Effect.Browser.Navigation.Key
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , view : Audio.Model FrontendMsg_ FrontendModel_ -> Browser.Document (Audio.Msg FrontendMsg_)
    , update :
        Audio.Msg FrontendMsg_
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , updateFromBackend :
        ToFrontend
        -> Audio.Model FrontendMsg_ FrontendModel_
        -> ( Audio.Model FrontendMsg_ FrontendModel_, Command FrontendOnly ToBackend (Audio.Msg FrontendMsg_) )
    , subscriptions :
        Audio.Model FrontendMsg_ FrontendModel_
        -> Subscription FrontendOnly (Audio.Msg FrontendMsg_)
    , onUrlRequest : Browser.UrlRequest -> Audio.Msg FrontendMsg_
    , onUrlChange : Url -> Audio.Msg FrontendMsg_
    }
app_ =
    Audio.lamderaFrontendWithAudio
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = \audioData msg model -> update audioData msg model
        , updateFromBackend = \_ msg model -> updateFromBackend msg model |> (\( a, b ) -> ( a, b, Audio.cmdNone ))
        , subscriptions = subscriptions
        , view = view
        , audio = audio
        , audioPort =
            { toJS = Command.sendToJs "audioPortToJS" Ports.audioPortToJS
            , fromJS = Subscription.fromJs "audioPortFromJS" Ports.audioPortFromJS
            }
        }


audio : AudioData -> FrontendModel_ -> Audio
audio audioData model =
    case model of
        Loaded loaded ->
            audioLoaded audioData loaded

        Loading _ ->
            Audio.silence


audioLoaded : AudioData -> FrontendLoaded -> Audio
audioLoaded audioData model =
    Audio.silence


volume : FrontendLoaded -> Point2d WorldUnit WorldUnit -> Float
volume model position =
    let
        boundingBox =
            viewBoundingBox model

        boundingBox2 =
            BoundingBox2d.offsetBy (Units.tileUnit -4) boundingBox |> Maybe.withDefault boundingBox
    in
    if BoundingBox2d.contains position boundingBox2 then
        1

    else
        let
            extrema =
                BoundingBox2d.extrema boundingBox2

            (Quantity minX) =
                extrema.minX

            (Quantity minY) =
                extrema.minY

            (Quantity maxX) =
                extrema.maxX

            (Quantity maxY) =
                extrema.maxY

            { x, y } =
                Point2d.unwrap position

            distance : Float
            distance =
                if x > minX && x < maxX then
                    min (abs (minY - y)) (abs (maxY - y))

                else
                    min (abs (minX - x)) (abs (maxX - x))
        in
        if distance > maxVolumeDistance then
            0

        else
            ((maxVolumeDistance - distance) / maxVolumeDistance) ^ 2


maxVolumeDistance : number
maxVolumeDistance =
    10


init : Url -> Effect.Browser.Navigation.Key -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
init url key =
    ( Loading
        { key = key
        , windowSize = ( Pixels.pixels 1920, Pixels.pixels 1080 )
        , cssWindowSize = Coord.xy 1920 1080
        , cssCanvasSize = Coord.xy 1920 1080
        , devicePixelRatio = 1
        , zoomFactor = 1
        , time = Nothing
        , viewPoint = Coord.origin
        , mousePosition = Point2d.origin
        , sounds = AssocList.empty
        , musicVolume = 0
        , soundEffectVolume = 0
        , texture = Nothing
        , lightsTexture = Nothing
        , depthTexture = Nothing
        , localModel = LoadingLocalModel []
        }
    , Command.batch
        [ Effect.Lamdera.sendToBackend ConnectToBackend
        , Effect.Task.perform
            (\{ viewport } -> WindowResized (Coord.xy (round viewport.width) (round viewport.height)))
            Effect.Browser.Dom.getViewport
        , Effect.WebGL.Texture.loadWith
            { magnify = Effect.WebGL.Texture.nearest
            , minify = Effect.WebGL.Texture.nearest
            , horizontalWrap = Effect.WebGL.Texture.clampToEdge
            , verticalWrap = Effect.WebGL.Texture.clampToEdge
            , flipY = False
            }
            "/texture.png"
            |> Effect.Task.attempt TextureLoaded
        , Effect.WebGL.Texture.loadWith
            { magnify = Effect.WebGL.Texture.nearest
            , minify = Effect.WebGL.Texture.nearest
            , horizontalWrap = Effect.WebGL.Texture.clampToEdge
            , verticalWrap = Effect.WebGL.Texture.clampToEdge
            , flipY = False
            }
            "/depth.png"
            |> Effect.Task.attempt DepthTextureLoaded
        ]
    , Audio.cmdNone
    )


update : AudioData -> FrontendMsg_ -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_, AudioCmd FrontendMsg_ )
update audioData msg model =
    case model of
        Loading loadingModel ->
            LoadingPage.update msg loadingModel

        Loaded frontendLoaded ->
            updateLoaded audioData msg frontendLoaded
                |> (\( a, b ) -> ( Loaded a, b, Audio.cmdNone ))


updateLoaded : AudioData -> FrontendMsg_ -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoaded audioData msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Command.batch [ Effect.Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                Browser.External url ->
                    ( model
                    , Effect.Browser.Navigation.load url
                    )

        UrlChanged url ->
            ( model
            , Command.none
            )

        NoOpFrontendMsg ->
            ( model, Command.none )

        TextureLoaded _ ->
            ( model, Command.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Command.none )

        KeyDown rawKey ->
            case Keyboard.anyKeyOriginal rawKey of
                Just key ->
                    case ( model.focus, key ) of
                        ( _, Keyboard.Tab ) ->
                            ( setFocus
                                (if LocalGrid.keyDown Keyboard.Shift model then
                                    previousFocus model

                                 else
                                    nextFocus model
                                )
                                model
                            , Command.none
                            )

                        ( _, Keyboard.F1 ) ->
                            ( { model | hideUi = not model.hideUi }, Command.none )

                        ( Just id, _ ) ->
                            uiUpdate audioData id (Ui.KeyDown rawKey key) model

                        _ ->
                            ( model, Command.none )

                Nothing ->
                    ( model, Command.none )

        WindowResized windowSize ->
            LoadingPage.windowResizedUpdate windowSize model

        GotDevicePixelRatio devicePixelRatio ->
            LoadingPage.devicePixelRatioChanged devicePixelRatio model

        MouseDown button mousePosition ->
            let
                hover =
                    LoadingPage.hoverAt model mousePosition
            in
            if button == MainButton then
                { model
                    | mouseLeft =
                        MouseButtonDown
                            { start = mousePosition
                            , start_ = Toolbar.screenToWorld model mousePosition
                            , current = mousePosition
                            , hover = hover
                            }
                    , focus = Nothing
                }
                    |> (\model2 ->
                            case hover of
                                UiHover id data ->
                                    uiUpdate
                                        audioData
                                        id
                                        (Ui.MouseDown { elementPosition = data.position })
                                        { model2 | focus = Just id }

                                _ ->
                                    ( model2, Command.none )
                       )

            else if button == MiddleButton then
                ( { model
                    | mouseMiddle =
                        MouseButtonDown
                            { start = mousePosition
                            , start_ = Toolbar.screenToWorld model mousePosition
                            , current = mousePosition
                            , hover = hover
                            }
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        MouseUp button mousePosition ->
            case ( button, model.mouseLeft, model.mouseMiddle ) of
                ( MainButton, MouseButtonDown previousMouseState, _ ) ->
                    mainMouseButtonUp audioData mousePosition previousMouseState model

                ( MiddleButton, _, MouseButtonDown mouseState ) ->
                    ( { model
                        | mouseMiddle = MouseButtonUp { current = mousePosition }
                      }
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        MouseLeave ->
            case model.mouseLeft of
                MouseButtonDown mouseState ->
                    mainMouseButtonUp audioData (LoadingPage.mouseScreenPosition model) mouseState model

                MouseButtonUp _ ->
                    ( model, Command.none )

        MouseMove mousePosition ->
            { model
                | mouseLeft =
                    case model.mouseLeft of
                        MouseButtonDown mouseState ->
                            MouseButtonDown { mouseState | current = mousePosition }

                        MouseButtonUp _ ->
                            MouseButtonUp { current = mousePosition }
                , mouseMiddle =
                    case model.mouseMiddle of
                        MouseButtonDown mouseState ->
                            MouseButtonDown { mouseState | current = mousePosition }

                        MouseButtonUp _ ->
                            MouseButtonUp { current = mousePosition }
            }
                |> (\model2 ->
                        case model2.mouseLeft of
                            MouseButtonDown { hover } ->
                                case hover of
                                    UiBackgroundHover ->
                                        ( model2, Command.none )

                                    UiHover uiHover data ->
                                        uiUpdate audioData uiHover (Ui.MouseMove { elementPosition = data.position }) model2

                                    MapHover ->
                                        ( model2, Command.none )

                            _ ->
                                ( model2, Command.none )
                   )

        AnimationFrame localTime ->
            let
                time =
                    Duration.addTo localTime (PingData.pingOffset model)

                model2 =
                    { model
                        | time = time
                        , localTime = localTime
                        , animationElapsedTime = Duration.from model.time time |> Quantity.plus model.animationElapsedTime
                    }

                newUi =
                    Toolbar.view model2 (LoadingPage.hoverAt model2 (LoadingPage.mouseScreenPosition model2))

                visuallyEqual =
                    Ui.visuallyEqual newUi model2.ui
            in
            ( { model2
                | ui = newUi
                , localModel =
                    LocalModel.unwrap model2.localModel
                        |> (\a -> a)
                        |> LocalModel.unsafe
                , previousFocus = model2.focus
                , focus =
                    if visuallyEqual then
                        model2.focus

                    else
                        case Maybe.andThen (\id -> Ui.findInput id newUi) model2.focus of
                            Just _ ->
                                model2.focus

                            Nothing ->
                                Nothing
                , uiMesh =
                    if visuallyEqual && model2.focus == model2.previousFocus then
                        model2.uiMesh

                    else
                        Ui.view model2.focus newUi
              }
            , Command.none
            )

        SoundLoaded sound result ->
            ( { model | sounds = AssocList.insert sound result model.sounds }, Command.none )

        TrainTextureLoaded result ->
            case result of
                Ok texture ->
                    ( { model | trainTexture = Just texture }, Command.none )

                Err _ ->
                    ( model, Command.none )

        TrainDepthTextureLoaded result ->
            case result of
                Ok texture ->
                    ( { model | trainDepthTexture = Just texture }, Command.none )

                Err _ ->
                    ( model, Command.none )

        DepthTextureLoaded _ ->
            ( model, Command.none )


previousFocus : FrontendLoaded -> Maybe UiHover
previousFocus model =
    case model.focus of
        Just hoverId ->
            Just (Ui.tabBackward hoverId model.ui)

        _ ->
            Nothing


nextFocus : FrontendLoaded -> Maybe UiHover
nextFocus model =
    case model.focus of
        Just hoverId ->
            Just (Ui.tabForward hoverId model.ui)

        _ ->
            Nothing


colorTextInputAdjustText : TextInput.Model -> TextInput.Model
colorTextInputAdjustText model =
    TextInput.replaceState
        (\a ->
            { text = String.left 6 a.text
            , cursorPosition = min 6 a.cursorPosition
            , cursorSize = a.cursorSize
            }
        )
        model


categoryHotkeys : Dict String Category
categoryHotkeys =
    Dict.fromList
        [ ( "s", Scenery )
        , ( "b", Buildings )
        , ( "t", Rail )
        , ( "r", Road )
        ]


isSmallDistance : { a | start : Point2d Pixels coordinates } -> Point2d Pixels coordinates -> Bool
isSmallDistance previousMouseState mousePosition =
    Vector2d.from previousMouseState.start mousePosition
        |> Vector2d.length
        |> Quantity.lessThan (Pixels.pixels 5)


mainMouseButtonUp :
    AudioData
    -> Point2d Pixels Pixels
    -> { a | start : Point2d Pixels Pixels, hover : Hover }
    -> FrontendLoaded
    -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
mainMouseButtonUp audioData mousePosition previousMouseState model =
    let
        isSmallDistance2 =
            isSmallDistance previousMouseState mousePosition

        hoverAt2 : Hover
        hoverAt2 =
            LoadingPage.hoverAt model mousePosition

        sameUiHover : Maybe UiHover
        sameUiHover =
            case ( hoverAt2, previousMouseState.hover ) of
                ( UiHover new _, UiHover old _ ) ->
                    if new == old then
                        Just new

                    else
                        Nothing

                _ ->
                    Nothing

        model2 =
            { model
                | mouseLeft = MouseButtonUp { current = mousePosition }
            }
                |> (\m ->
                        case sameUiHover of
                            Just uiHover ->
                                setFocus (Just uiHover) m

                            Nothing ->
                                m
                   )
    in
    case hoverAt2 of
        UiBackgroundHover ->
            ( model2, Command.none )

        MapHover ->
            LoadingPage.updateLocalModel
                (Change.WalkTowards (Toolbar.screenToWorld model mousePosition))
                model
                |> LoadingPage.handleOutMsg False

        UiHover id _ ->
            case sameUiHover of
                Just _ ->
                    uiUpdate audioData id Ui.MousePressed model2

                Nothing ->
                    ( model2, Command.none )


onPress :
    AudioData
    -> UiEvent
    -> (() -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ ))
    -> FrontendLoaded
    -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
onPress audioData event updateFunc model =
    case event of
        Ui.MousePressed ->
            updateFunc ()

        Ui.KeyDown _ Keyboard.Enter ->
            updateFunc ()

        Ui.KeyDown rawKey key ->
            ( model, Command.none )

        _ ->
            ( model, Command.none )


uiUpdate : AudioData -> UiHover -> UiEvent -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
uiUpdate audioData id event model =
    case id of
        DefaultButton ->
            onPress audioData event (\() -> ( model, Command.none )) model


textInputUpdate :
    Int
    -> UiHover
    -> (TextInput.Model -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly toMsg msg ))
    -> (() -> ( FrontendLoaded, Command FrontendOnly toMsg msg ))
    -> TextInput.Model
    -> (TextInput.Model -> FrontendLoaded)
    -> UiEvent
    -> FrontendLoaded
    -> ( FrontendLoaded, Command FrontendOnly toMsg msg )
textInputUpdate textScale id textChanged onEnter textInput setTextInput event model =
    case event of
        Ui.PastedText text ->
            let
                textInput2 =
                    TextInput.paste text textInput
            in
            setTextInput textInput2 |> textChanged textInput2

        Ui.MouseDown { elementPosition } ->
            ( TextInput.mouseDown
                textScale
                (LoadingPage.mouseScreenPosition model |> Coord.roundPoint)
                elementPosition
                textInput
                |> setTextInput
                |> setFocus (Just id)
            , Command.none
            )

        Ui.KeyDown _ Keyboard.Escape ->
            ( setFocus Nothing model, Command.none )

        Ui.KeyDown _ Keyboard.Enter ->
            onEnter ()

        Ui.KeyDown _ key ->
            let
                ( newTextInput, outMsg ) =
                    TextInput.keyMsg
                        (LocalGrid.ctrlOrMeta model)
                        (LocalGrid.keyDown Keyboard.Shift model)
                        key
                        textInput

                ( model2, cmd ) =
                    setTextInput newTextInput |> textChanged newTextInput
            in
            ( model2
            , case outMsg of
                CopyText text ->
                    cmd

                PasteText ->
                    cmd

                NoOutMsg ->
                    cmd
            )

        Ui.MousePressed ->
            ( model, Command.none )

        Ui.MouseMove { elementPosition } ->
            case model.mouseLeft of
                MouseButtonDown { current } ->
                    ( TextInput.mouseDownMove textScale (Coord.roundPoint current) elementPosition textInput
                        |> setTextInput
                    , Command.none
                    )

                MouseButtonUp _ ->
                    ( model, Command.none )


textInputMultilineUpdate :
    Int
    -> Int
    -> UiHover
    -> (TextInputMultiline.Model -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly toMsg msg ))
    -> TextInputMultiline.Model
    -> (TextInputMultiline.Model -> FrontendLoaded)
    -> UiEvent
    -> FrontendLoaded
    -> ( FrontendLoaded, Command FrontendOnly toMsg msg )
textInputMultilineUpdate textScale width id textChanged textInput setTextInput event model =
    case event of
        Ui.PastedText text ->
            let
                textInput2 =
                    TextInputMultiline.paste text textInput
            in
            setTextInput textInput2 |> textChanged textInput2

        Ui.MouseDown { elementPosition } ->
            ( TextInputMultiline.mouseDown
                textScale
                (LoadingPage.mouseScreenPosition model |> Coord.roundPoint)
                elementPosition
                textInput
                |> setTextInput
                |> setFocus (Just id)
            , Command.none
            )

        Ui.KeyDown _ Keyboard.Escape ->
            ( setFocus Nothing model, Command.none )

        Ui.KeyDown _ key ->
            let
                ( newTextInput, outMsg ) =
                    TextInputMultiline.keyMsg
                        textScale
                        width
                        (LocalGrid.ctrlOrMeta model)
                        (LocalGrid.keyDown Keyboard.Shift model)
                        key
                        textInput

                ( model2, cmd ) =
                    setTextInput newTextInput |> textChanged newTextInput
            in
            ( model2
            , case outMsg of
                CopyText text ->
                    cmd

                PasteText ->
                    cmd

                NoOutMsg ->
                    cmd
            )

        Ui.MousePressed ->
            ( model, Command.none )

        Ui.MouseMove { elementPosition } ->
            case model.mouseLeft of
                MouseButtonDown { current } ->
                    ( TextInputMultiline.mouseDownMove textScale (Coord.roundPoint current) elementPosition textInput
                        |> setTextInput
                    , Command.none
                    )

                MouseButtonUp _ ->
                    ( model, Command.none )


setFocus : Maybe UiHover -> FrontendLoaded -> FrontendLoaded
setFocus newFocus model =
    { model
        | focus = newFocus
    }


updateFromBackend : ToFrontend -> FrontendModel_ -> ( FrontendModel_, Command FrontendOnly ToBackend FrontendMsg_ )
updateFromBackend msg model =
    case ( model, msg ) of
        ( Loading loading, LoadingData loadingData ) ->
            ( Loading
                { loading
                    | localModel =
                        case loading.localModel of
                            LoadingLocalModel [] ->
                                LoadedLocalModel
                                    { localModel = LocalGrid.init loadingData
                                    }

                            LoadingLocalModel (first :: rest) ->
                                LoadedLocalModel
                                    { localModel =
                                        LocalGrid.init loadingData
                                            |> LocalGrid.updateFromBackend (Nonempty first rest)
                                            |> Tuple.first
                                    }

                            LoadedLocalModel _ ->
                                loading.localModel
                }
            , Command.none
            )

        ( Loading loading, ChangeBroadcast changes ) ->
            ( (case loading.localModel of
                LoadingLocalModel pendingChanges ->
                    { loading | localModel = pendingChanges ++ List.Nonempty.toList changes |> LoadingLocalModel }

                LoadedLocalModel loadedLocalModel ->
                    { loading
                        | localModel =
                            LoadedLocalModel
                                { localModel = LocalGrid.updateFromBackend changes loadedLocalModel.localModel |> Tuple.first
                                }
                    }
              )
                |> Loading
            , Command.none
            )

        ( Loaded loaded, _ ) ->
            updateLoadedFromBackend msg loaded
                |> Tuple.mapFirst Loaded

        _ ->
            ( model, Command.none )


updateLoadedFromBackend : ToFrontend -> FrontendLoaded -> ( FrontendLoaded, Command FrontendOnly ToBackend FrontendMsg_ )
updateLoadedFromBackend msg model =
    case msg of
        LoadingData loadingData ->
            ( { model
                | localModel = LocalGrid.init loadingData
                , isReconnecting = False
                , pendingChanges = []
              }
            , Command.none
            )

        ChangeBroadcast changes ->
            let
                ( newLocalModel, outMsgs ) =
                    LocalGrid.updateFromBackend changes model.localModel
            in
            List.foldl
                (\outMsg ( state, cmd ) ->
                    let
                        ( model2, cmd2 ) =
                            LoadingPage.handleOutMsg True ( state, outMsg )
                    in
                    ( model2, Command.batch [ cmd, cmd2 ] )
                )
                ( { model | localModel = newLocalModel }, Command.none )
                outMsgs

        PingResponse serverTime ->
            case model.pingStartTime of
                Just pingStartTime ->
                    let
                        keepPinging =
                            (pingCount < 5)
                                || (newHighEstimate
                                        |> Quantity.minus newLowEstimate
                                        |> Quantity.greaterThan (Duration.milliseconds 200)
                                   )

                        {- The time stored in the model is potentially out of date by an animation frame. We want to make sure our high estimate overestimates rather than underestimates the true time so we add an extra animation frame here. -}
                        localTimeHighEstimate =
                            Duration.addTo (actualTime model) (Duration.milliseconds (1000 / 60))

                        serverTime2 =
                            serverTime

                        ( newLowEstimate, newHighEstimate, pingCount ) =
                            case model.pingData of
                                Just oldPingData ->
                                    ( Duration.from serverTime2 pingStartTime |> Quantity.max oldPingData.lowEstimate
                                    , Duration.from serverTime2 localTimeHighEstimate |> Quantity.min oldPingData.highEstimate
                                    , oldPingData.pingCount + 1
                                    )

                                Nothing ->
                                    ( Duration.from serverTime2 pingStartTime
                                    , Duration.from serverTime2 localTimeHighEstimate
                                    , 1
                                    )
                    in
                    ( { model
                        | pingData =
                            -- This seems to happen if the user tabs away. I'm not sure how to prevent it so here we just start over if we end up in this state.
                            if newHighEstimate |> Quantity.lessThan newLowEstimate then
                                Nothing

                            else
                                Just
                                    { roundTripTime = Duration.from pingStartTime (actualTime model)
                                    , lowEstimate = newLowEstimate
                                    , highEstimate = newHighEstimate
                                    , serverTime = serverTime2
                                    , sendTime = pingStartTime
                                    , receiveTime = actualTime model
                                    , pingCount = pingCount
                                    }
                        , pingStartTime =
                            if keepPinging then
                                Just (actualTime model)

                            else
                                Nothing
                      }
                    , if keepPinging then
                        Effect.Lamdera.sendToBackend PingRequest

                      else
                        Command.none
                    )

                Nothing ->
                    ( model, Command.none )

        ClientConnected ->
            ( { model | isReconnecting = True }
            , ConnectToBackend |> Effect.Lamdera.sendToBackend
            )

        CheckConnectionBroadcast ->
            ( { model | lastCheckConnection = model.time }, Command.none )


actualTime : FrontendLoaded -> Effect.Time.Posix
actualTime model =
    Duration.addTo model.localTime debugTimeOffset


debugTimeOffset : Duration
debugTimeOffset =
    Duration.seconds 0


view : AudioData -> FrontendModel_ -> Browser.Document FrontendMsg_
view audioData model =
    { title = "Snowball fight"
    , body =
        [ case model of
            Loading loadingModel ->
                LoadingPage.loadingCanvasView loadingModel

            Loaded loadedModel ->
                canvasView audioData loadedModel
        , Html.node "style" [] [ Html.text "body { overflow: hidden; margin: 0; }" ]
        ]
    }


viewBoundingBox : FrontendLoaded -> BoundingBox2d WorldUnit WorldUnit
viewBoundingBox model =
    BoundingBox2d.from
        (Toolbar.screenToWorld model Point2d.origin)
        (Toolbar.screenToWorld model (Coord.toPoint2d model.windowSize))


shaderTime : { a | startTime : Time.Posix, time : Time.Posix } -> Float
shaderTime model =
    Duration.from model.startTime model.time |> Duration.inSeconds


uiNightFactorScaling : Float
uiNightFactorScaling =
    0.3


staticMatrix : Int -> Int -> Int -> Mat4
staticMatrix windowWidth windowHeight zoom =
    Mat4.makeScale3 (toFloat zoom * 2 / toFloat windowWidth) (toFloat zoom * -2 / toFloat windowHeight) 1


canvasView : AudioData -> FrontendLoaded -> Html FrontendMsg_
canvasView audioData model =
    case
        ( Effect.WebGL.Texture.unwrap model.texture
        , Effect.WebGL.Texture.unwrap model.depthTexture
        , ( Maybe.andThen Effect.WebGL.Texture.unwrap model.trainTexture
          , Maybe.andThen Effect.WebGL.Texture.unwrap model.trainDepthTexture
          )
        )
    of
        ( Just texture, Just depth, ( Just playerTexture, Just playerDepth ) ) ->
            let
                viewBounds_ : BoundingBox2d WorldUnit WorldUnit
                viewBounds_ =
                    viewBoundingBox model

                ( windowWidth, windowHeight ) =
                    Coord.toTuple model.windowSize

                ( cssWindowWidth, cssWindowHeight ) =
                    Coord.toTuple model.cssCanvasSize

                { x, y } =
                    Point2d.unwrap (Toolbar.actualViewPoint model.viewPoint)

                hoverAt2 : Hover
                hoverAt2 =
                    LoadingPage.hoverAt model (LoadingPage.mouseScreenPosition model)

                staticViewMatrix =
                    staticMatrix windowWidth windowHeight model.zoomFactor

                renderData : RenderData
                renderData =
                    { texture = texture
                    , depth = depth
                    , nightFactor = 0
                    , staticViewMatrix = staticViewMatrix
                    , viewMatrix =
                        staticViewMatrix
                            |> Mat4.translate3
                                (negate <| toFloat <| round (x * toFloat Units.tileWidth))
                                (negate <| toFloat <| round (y * toFloat Units.tileHeight))
                                0
                    , time = shaderTime model
                    , scissors = { left = 0, bottom = 0, width = windowWidth, height = windowHeight }
                    }

                textureSize : Vec2
                textureSize =
                    WebGL.Texture.size texture |> Coord.tuple |> Coord.toVec2

                localModel : LocalGrid_
                localModel =
                    LocalGrid.localModel model.localModel
            in
            Effect.WebGL.toHtmlWith
                [ Effect.WebGL.alpha False
                , Effect.WebGL.clearColor 1 1 1 1
                , Effect.WebGL.depth 1
                ]
                ([ Html.Attributes.width windowWidth
                 , Html.Attributes.height windowHeight
                 , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
                 , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
                 , Html.Events.preventDefaultOn "keydown" (Json.Decode.succeed ( NoOpFrontendMsg, True ))
                 ]
                    ++ LoadingPage.mouseListeners model
                )
                ((case IdDict.get localModel.userId localModel.users of
                    Just user ->
                        [ Effect.WebGL.entity
                            Shaders.vertexShader
                            Shaders.fragmentShader
                            (playerSprite user.position)
                            { view = renderData.viewMatrix
                            , textureSize = textureSize
                            , time = renderData.time
                            , texture = playerTexture
                            , night = 0
                            , depth = playerDepth
                            , color = Math.Vector4.vec4 1 1 1 1
                            }
                        ]

                    Nothing ->
                        []
                 )
                    ++ [ Effect.WebGL.entity
                            Shaders.vertexShader
                            Shaders.fragmentShader
                            treeSprite
                            { view = renderData.viewMatrix
                            , textureSize = textureSize
                            , time = renderData.time
                            , texture = renderData.texture
                            , night = 0
                            , depth = renderData.depth
                            , color = Math.Vector4.vec4 1 1 1 1
                            }
                       ]
                )

        _ ->
            Html.text ""


treeSprite : Mesh Vertex
treeSprite =
    Sprite.sprite (Coord.xy -100 0) (Coord.xy 60 54) (Coord.xy 640 756) (Coord.xy 60 54) |> Sprite.toMesh


playerSprite : Point2d WorldUnit WorldUnit -> Mesh Vertex
playerSprite position =
    Sprite.sprite
        (Units.tileToPixelPoint position |> Coord.floorPoint)
        (Coord.xy 60 70)
        (Coord.xy 0 0)
        (Coord.xy 60 70)
        |> Sprite.toMesh


subscriptions : AudioData -> FrontendModel_ -> Subscription FrontendOnly FrontendMsg_
subscriptions _ model =
    Subscription.batch
        [ Ports.gotDevicePixelRatio GotDevicePixelRatio
        , Effect.Browser.Events.onResize (\width height -> WindowResized (Coord.xy width height))
        , Effect.Browser.Events.onAnimationFrame AnimationFrame
        , Keyboard.downs KeyDown
        , case model of
            Loading _ ->
                Subscription.none

            Loaded loaded ->
                Subscription.batch
                    [ Subscription.map KeyMsg Keyboard.subscriptions
                    ]
        , Subscription.fromJs "mouse_leave" Ports.mouse_leave (\_ -> MouseLeave)
        ]
