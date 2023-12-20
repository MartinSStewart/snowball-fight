module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , ContextMenu
    , CssPixels
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel
    , FrontendModel_(..)
    , FrontendMsg
    , FrontendMsg_(..)
    , Hover(..)
    , LoadedLocalModel_
    , LoadingData_
    , LoadingLocalModel(..)
    , MouseButtonState(..)
    , RemovedTileParticle
    , SubmitStatus(..)
    , ToBackend(..)
    , ToFrontend(..)
    , ToolButton(..)
    , TopMenu(..)
    , UiHover(..)
    , UserSettings
    , ViewPoint(..)
    )

import AssocList
import AssocSet
import Audio
import Browser
import Change exposing (AreTrainsAndAnimalsDisabled, BackendReport, Change, UserStatus)
import Color exposing (Colors)
import Coord exposing (Coord, RawCellCoord)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import Effect.WebGL.Texture exposing (Texture)
import Html.Events.Extra.Mouse exposing (Button)
import Id exposing (AnimalId, EventId, Id, MailId, OneTimePasswordId, PersonId, SecretId, TrainId, UserId)
import IdDict exposing (IdDict)
import Keyboard
import Lamdera
import List.Nonempty exposing (Nonempty)
import LocalGrid exposing (LocalGrid)
import LocalModel exposing (LocalModel)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Sound exposing (Sound)
import Sprite exposing (Vertex)
import TextInput
import Tile exposing (Category, Tile, TileGroup)
import Time
import Ui
import Units exposing (CellUnit, WorldUnit)
import Url exposing (Url)
import User exposing (FrontendUser)
import WebGL


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { key : Effect.Browser.Navigation.Key
    , windowSize : Coord Pixels
    , cssWindowSize : Coord CssPixels
    , cssCanvasSize : Coord CssPixels
    , devicePixelRatio : Float
    , zoomFactor : Int
    , time : Maybe Effect.Time.Posix
    , viewPoint : Coord WorldUnit
    , mousePosition : Point2d Pixels Pixels
    , sounds : AssocList.Dict Sound (Result Audio.LoadError Audio.Source)
    , musicVolume : Int
    , soundEffectVolume : Int
    , texture : Maybe Texture
    , lightsTexture : Maybe Texture
    , depthTexture : Maybe Texture
    , localModel : LoadingLocalModel
    }


type LoadingLocalModel
    = LoadingLocalModel (List Change)
    | LoadedLocalModel LoadedLocalModel_


type alias LoadedLocalModel_ =
    { localModel : LocalModel Change LocalGrid
    }


type ViewPoint
    = NormalViewPoint (Point2d WorldUnit WorldUnit)


type ToolButton
    = HandToolButton
    | TilePlacerToolButton TileGroup
    | TilePickerToolButton
    | TextToolButton
    | ReportToolButton


type alias FrontendLoaded =
    { key : Effect.Browser.Navigation.Key
    , localModel : LocalModel Change LocalGrid
    , viewPoint : ViewPoint
    , texture : Texture
    , depthTexture : Texture
    , trainTexture : Maybe Texture
    , trainDepthTexture : Maybe Texture
    , pressedKeys : List Keyboard.Key
    , windowSize : Coord Pixels
    , cssWindowSize : Coord CssPixels
    , cssCanvasSize : Coord CssPixels
    , devicePixelRatio : Float
    , zoomFactor : Int
    , mouseLeft : MouseButtonState
    , mouseMiddle : MouseButtonState
    , pendingChanges : List ( Id EventId, Change.LocalChange )
    , time : Effect.Time.Posix
    , startTime : Effect.Time.Posix
    , animationElapsedTime : Duration
    , sounds : AssocList.Dict Sound (Result Audio.LoadError Audio.Source)
    , musicVolume : Int
    , soundEffectVolume : Int
    , ui : Ui.Element UiHover
    , uiMesh : WebGL.Mesh Vertex
    , eventIdCounter : Id EventId
    , pingData : Maybe PingData
    , pingStartTime : Maybe Effect.Time.Posix
    , localTime : Effect.Time.Posix
    , previousFocus : Maybe UiHover
    , focus : Maybe UiHover
    , topMenuOpened : Maybe TopMenu
    , isReconnecting : Bool
    , lastCheckConnection : Time.Posix
    , hideUi : Bool
    }


type alias ContextMenu =
    { change :
        Maybe
            { userId : Id UserId
            , tile : Tile
            , position : Coord WorldUnit
            , colors : Colors
            , time : Effect.Time.Posix
            }
    , position : Coord WorldUnit
    , linkCopied : Bool
    }


type TopMenu
    = SettingsMenu TextInput.Model
    | LoggedOutSettingsMenu


type SubmitStatus a
    = NotSubmitted { pressedSubmit : Bool }
    | Submitting
    | Submitted a


type alias RemovedTileParticle =
    { time : Effect.Time.Posix, position : Coord WorldUnit, tile : Tile, colors : Colors }


type MouseButtonState
    = MouseButtonUp { current : Point2d Pixels Pixels }
    | MouseButtonDown
        { start : Point2d Pixels Pixels
        , start_ : Point2d WorldUnit WorldUnit
        , current : Point2d Pixels Pixels
        , hover : Hover
        }


type alias UserSettings =
    { musicVolume : Int, soundEffectVolume : Int }


type Hover
    = MapHover
    | UiBackgroundHover
    | UiHover UiHover { position : Coord Pixels }


type UiHover
    = DefaultButton


type alias BackendModel =
    { userSessions :
        Dict
            Lamdera.SessionId
            { clientIds : AssocSet.Set ClientId
            , userId : Id UserId
            , user : FrontendUser
            }
    , userIdCounter : Int
    }


type CssPixels
    = CssPixel Never


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | TextureLoaded (Result Effect.WebGL.Texture.Error Texture)
    | DepthTextureLoaded (Result Effect.WebGL.Texture.Error Texture)
    | TrainTextureLoaded (Result Effect.WebGL.Texture.Error Texture)
    | TrainDepthTextureLoaded (Result Effect.WebGL.Texture.Error Texture)
    | KeyMsg Keyboard.Msg
    | KeyDown Keyboard.RawKey
    | WindowResized (Coord CssPixels)
    | GotDevicePixelRatio Float
    | MouseDown Button (Point2d Pixels Pixels)
    | MouseUp Button (Point2d Pixels Pixels)
    | MouseMove (Point2d Pixels Pixels)
    | MouseLeave
    | AnimationFrame Effect.Time.Posix
    | SoundLoaded Sound (Result Audio.LoadError Audio.Source)


type ToBackend
    = ConnectToBackend
    | PingRequest


type BackendMsg
    = UserDisconnected SessionId ClientId
    | UserConnected ClientId
    | UpdateFromFrontend SessionId ClientId ToBackend Effect.Time.Posix


type ToFrontend
    = LoadingData LoadingData_
    | ChangeBroadcast (Nonempty Change)
    | PingResponse Effect.Time.Posix
    | ClientConnected
    | CheckConnectionBroadcast


type alias LoadingData_ =
    { userId : Id UserId
    , users : IdDict UserId FrontendUser
    }
