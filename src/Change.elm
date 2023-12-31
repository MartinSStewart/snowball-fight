module Change exposing
    ( AdminChange(..)
    , AdminData
    , AreTrainsAndAnimalsDisabled(..)
    , BackendReport
    , Change(..)
    , LocalChange(..)
    , LoggedIn_
    , MovementChange
    , NotLoggedIn_
    , Report
    , ServerChange(..)
    , TileHotkey(..)
    , UserStatus(..)
    , tileHotkeyDict
    )

import Animal exposing (Animal)
import Array exposing (Array)
import AssocList
import Bounds exposing (Bounds)
import Coord exposing (Coord, RawCellCoord)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Effect.Time
import EmailAddress exposing (EmailAddress)
import Id exposing (AnimalId, EventId, Id, MailId, TrainId, UserId)
import IdDict exposing (IdDict)
import List.Nonempty exposing (Nonempty)
import MailEditor exposing (BackendMail, MailStatus)
import Point2d exposing (Point2d)
import Set exposing (Set)
import Tile exposing (TileGroup)
import TimeOfDay exposing (TimeOfDay)
import Units exposing (CellUnit, WorldUnit)


type Change
    = LocalChange (Id EventId) LocalChange
    | ServerChange ServerChange


type LocalChange
    = SelectCharacter
    | InvalidChange
    | WalkTowards (Point2d WorldUnit WorldUnit)


tileHotkeyDict : Dict String TileHotkey
tileHotkeyDict =
    Dict.fromList
        [ ( "Digit0", Hotkey0 )
        , ( "Digit1", Hotkey1 )
        , ( "Digit2", Hotkey2 )
        , ( "Digit3", Hotkey3 )
        , ( "Digit4", Hotkey4 )
        , ( "Digit5", Hotkey5 )
        , ( "Digit6", Hotkey6 )
        , ( "Digit7", Hotkey7 )
        , ( "Digit8", Hotkey8 )
        , ( "Digit9", Hotkey9 )
        ]


type TileHotkey
    = Hotkey0
    | Hotkey1
    | Hotkey2
    | Hotkey3
    | Hotkey4
    | Hotkey5
    | Hotkey6
    | Hotkey7
    | Hotkey8
    | Hotkey9


type AdminChange
    = AdminResetSessions
    | AdminSetGridReadOnly Bool
    | AdminSetTrainsDisabled AreTrainsAndAnimalsDisabled
    | AdminDeleteMail (Id MailId) Effect.Time.Posix
    | AdminRestoreMail (Id MailId)
    | AdminResetUpdateDuration
    | AdminRegenerateGridCellCache Effect.Time.Posix


type AreTrainsAndAnimalsDisabled
    = TrainsAndAnimalsDisabled
    | TrainsAndAnimalsEnabled


type ServerChange
    = ServerUserDisconnected (Id UserId)
    | ServerUserConnected (Id UserId) (Point2d WorldUnit WorldUnit)
    | ServerWalkTowards (Id UserId) (Point2d WorldUnit WorldUnit)


type alias MovementChange =
    { startTime : Effect.Time.Posix
    , position : Point2d WorldUnit WorldUnit
    , endPosition : Point2d WorldUnit WorldUnit
    }


type UserStatus
    = LoggedIn LoggedIn_
    | NotLoggedIn NotLoggedIn_


type alias NotLoggedIn_ =
    { timeOfDay : TimeOfDay }


type alias LoggedIn_ =
    { userId : Id UserId
    , undoHistory : List (Dict RawCellCoord Int)
    , redoHistory : List (Dict RawCellCoord Int)
    , undoCurrent : Dict RawCellCoord Int
    , mailDrafts : IdDict UserId (List MailEditor.Content)
    , emailAddress : EmailAddress
    , inbox : IdDict MailId MailEditor.ReceivedMail
    , allowEmailNotifications : Bool
    , adminData : Maybe AdminData
    , reports : List Report
    , isGridReadOnly : Bool
    , timeOfDay : TimeOfDay
    , tileHotkeys : AssocList.Dict TileHotkey TileGroup
    , showNotifications : Bool
    , notifications : List (Coord WorldUnit)
    , notificationsClearedAt : Effect.Time.Posix
    , hyperlinksVisited : Set String
    }


type alias Report =
    { reportedUser : Id UserId, position : Coord WorldUnit }


type alias AdminData =
    { lastCacheRegeneration : Maybe Effect.Time.Posix
    , userSessions : List { userId : Maybe (Id UserId), connectionCount : Int }
    , reported : IdDict UserId (Nonempty BackendReport)
    , mail : IdDict MailId BackendMail
    , worldUpdateDurations : Array Duration
    , totalGridCells : Int
    }


type alias BackendReport =
    { reportedUser : Id UserId, position : Coord WorldUnit, reportedAt : Effect.Time.Posix }
