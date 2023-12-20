module LocalGrid exposing
    ( LocalGrid
    , LocalGrid_
    , OutMsg(..)
    , addNotification
    , addReported
    , ctrlOrMeta
    , deleteMail
    , init
    , keyDown
    , localModel
    , notificationViewportHalfSize
    , notificationViewportSize
    , removeReported
    , restoreMail
    , setTileHotkey
    , update
    , updateAnimalMovement
    , updateFromBackend
    , updateWorldUpdateDurations
    )

import Animal exposing (Animal, AnimalType(..))
import Array exposing (Array)
import AssocList
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox2dExtra
import Bounds exposing (Bounds)
import Change exposing (AdminChange(..), AdminData, AreTrainsAndAnimalsDisabled, BackendReport, Change(..), LocalChange(..), ServerChange(..), TileHotkey, UserStatus(..))
import Coord exposing (Coord, RawCellCoord)
import Duration exposing (Duration)
import Effect.Time
import Id exposing (AnimalId, Id, MailId, TrainId, UserId)
import IdDict exposing (IdDict)
import Keyboard
import LineSegment2d
import List.Nonempty exposing (Nonempty)
import LocalModel exposing (LocalModel)
import MailEditor exposing (FrontendMail, MailStatus(..), MailStatus2(..))
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Random
import Terrain exposing (TerrainType(..))
import Tile exposing (Tile, TileGroup)
import Units exposing (CellLocalUnit, CellUnit, WorldUnit)
import User exposing (FrontendUser, InviteTree)
import Vector2d exposing (Vector2d)


type LocalGrid
    = LocalGrid LocalGrid_


type alias LocalGrid_ =
    { userId : Id UserId
    , users : IdDict UserId FrontendUser
    }


ctrlOrMeta : { a | pressedKeys : List Keyboard.Key } -> Bool
ctrlOrMeta model =
    keyDown Keyboard.Control model || keyDown Keyboard.Meta model


keyDown : Keyboard.Key -> { a | pressedKeys : List Keyboard.Key } -> Bool
keyDown key { pressedKeys } =
    List.any ((==) key) pressedKeys


localModel : LocalModel a LocalGrid -> LocalGrid_
localModel localModel_ =
    LocalModel.localModel localModel_ |> (\(LocalGrid a) -> a)


init :
    { a
        | userId : Id UserId
        , users : IdDict UserId FrontendUser
    }
    -> LocalModel Change LocalGrid
init { userId, users } =
    LocalGrid
        { userId = userId
        , users = users
        }
        |> LocalModel.init


update : Change -> LocalModel Change LocalGrid -> ( LocalModel Change LocalGrid, OutMsg )
update change localModel_ =
    LocalModel.update config change localModel_


updateFromBackend : Nonempty Change -> LocalModel Change LocalGrid -> ( LocalModel Change LocalGrid, List OutMsg )
updateFromBackend changes localModel_ =
    LocalModel.updateFromBackend config changes localModel_


type OutMsg
    = NoOutMsg


updateLocalChange : LocalChange -> LocalGrid_ -> ( LocalGrid_, OutMsg )
updateLocalChange localChange model =
    case localChange of
        InvalidChange ->
            ( model, NoOutMsg )

        SelectCharacter ->
            ( model, NoOutMsg )

        WalkTowards position ->
            ( model, NoOutMsg )


resetUpdateDuration : AdminData -> AdminData
resetUpdateDuration adminData =
    { adminData | worldUpdateDurations = Array.empty }


updateAnimalMovement :
    { a | position : Coord WorldUnit, change : Tile, time : Effect.Time.Posix }
    -> IdDict AnimalId Animal
    -> IdDict AnimalId Animal
updateAnimalMovement change animals =
    IdDict.map
        (\_ animal ->
            let
                size : Vector2d WorldUnit WorldUnit
                size =
                    (Animal.getData animal.animalType).size
                        |> Units.pixelToTileVector
                        |> Vector2d.scaleBy 0.5
                        |> Vector2d.plus (Vector2d.xy Animal.moveCollisionThreshold Animal.moveCollisionThreshold)

                position : Point2d WorldUnit WorldUnit
                position =
                    Animal.actualPositionWithoutCursor change.time animal

                changeBounds =
                    Tile.worldMovementBounds size change.change change.position

                inside =
                    List.filter (BoundingBox2d.contains position) changeBounds
            in
            if List.isEmpty inside then
                let
                    maybeIntersection : Maybe (Point2d WorldUnit WorldUnit)
                    maybeIntersection =
                        List.concatMap
                            (\bounds ->
                                BoundingBox2dExtra.lineIntersection (LineSegment2d.from position animal.endPosition) bounds
                            )
                            changeBounds
                            |> Quantity.minimumBy (Point2d.distanceFrom position)
                in
                case maybeIntersection of
                    Just intersection ->
                        { animalType = animal.animalType
                        , position = animal.position
                        , startTime = animal.startTime
                        , endPosition = intersection
                        }

                    Nothing ->
                        animal

            else
                let
                    movedTo =
                        moveOutOfCollision position changeBounds
                in
                { animalType = animal.animalType
                , position = movedTo
                , startTime = animal.startTime
                , endPosition = movedTo
                }
        )
        animals


setTileHotkey :
    TileHotkey
    -> TileGroup
    -> { c | tileHotkeys : AssocList.Dict TileHotkey TileGroup }
    -> { c | tileHotkeys : AssocList.Dict TileHotkey TileGroup }
setTileHotkey hotkey tileGroup user =
    { user
        | tileHotkeys =
            AssocList.filter (\_ value -> value /= tileGroup) user.tileHotkeys
                |> AssocList.insert hotkey tileGroup
    }


viewMail :
    Id MailId
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
viewMail mailId model =
    { model
        | mail =
            IdDict.update2
                mailId
                (\mail ->
                    { mail
                        | status =
                            case mail.status of
                                MailReceived data ->
                                    MailReceivedAndViewed data

                                _ ->
                                    mail.status
                    }
                )
                model.mail
    }


restoreMail :
    Id MailId
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
restoreMail mailId model =
    { model
        | mail =
            IdDict.update2
                mailId
                (\mail ->
                    case mail.status of
                        MailDeletedByAdmin deleted ->
                            { mail
                                | status =
                                    case deleted.previousStatus of
                                        MailWaitingPickup2 ->
                                            MailWaitingPickup

                                        MailInTransit2 _ ->
                                            MailWaitingPickup

                                        MailReceived2 record ->
                                            MailReceived record

                                        MailReceivedAndViewed2 record ->
                                            MailReceivedAndViewed record
                            }

                        _ ->
                            mail
                )
                model.mail
    }


deleteMail :
    Id MailId
    -> Effect.Time.Posix
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
    -> { b | mail : IdDict MailId { c | status : MailStatus } }
deleteMail mailId time model =
    { model
        | mail =
            IdDict.update2
                mailId
                (\mail ->
                    { mail
                        | status =
                            case mail.status of
                                MailWaitingPickup ->
                                    MailDeletedByAdmin
                                        { previousStatus = MailWaitingPickup2
                                        , deletedAt = time
                                        }

                                MailInTransit id ->
                                    MailDeletedByAdmin
                                        { previousStatus = MailInTransit2 id
                                        , deletedAt = time
                                        }

                                MailReceived record ->
                                    MailDeletedByAdmin
                                        { previousStatus = MailReceived2 record
                                        , deletedAt = time
                                        }

                                MailReceivedAndViewed record ->
                                    MailDeletedByAdmin
                                        { previousStatus = MailReceivedAndViewed2 record
                                        , deletedAt = time
                                        }

                                MailDeletedByAdmin _ ->
                                    mail.status
                    }
                )
                model.mail
    }


notificationViewportHalfSize : Coord WorldUnit
notificationViewportHalfSize =
    Coord.xy 16 16


notificationViewportSize : Coord WorldUnit
notificationViewportSize =
    Coord.scalar 2 notificationViewportHalfSize


addNotification : Coord WorldUnit -> List (Coord WorldUnit) -> List (Coord WorldUnit)
addNotification position notifications =
    let
        bounds =
            Bounds.fromCoordAndSize
                (position |> Coord.minus notificationViewportHalfSize)
                notificationViewportSize
    in
    if
        List.any
            (\coord -> Bounds.contains coord bounds)
            notifications
    then
        notifications

    else
        position :: notifications


updateServerChange : ServerChange -> LocalGrid_ -> ( LocalGrid_, OutMsg )
updateServerChange serverChange model =
    case serverChange of
        ServerUserDisconnected userId ->
            ( { model | users = IdDict.remove userId model.users }
            , NoOutMsg
            )

        ServerUserConnected userId position ->
            ( { model
                | users =
                    IdDict.insert
                        userId
                        { position = position
                        , walkingTowards = Nothing
                        }
                        model.users
              }
            , NoOutMsg
            )

        ServerWalkTowards userId position ->
            ( { model
                | users =
                    IdDict.update
                        userId
                        (Maybe.map
                            (\user ->
                                { position = user.position
                                , walkingTowards = Just position
                                }
                            )
                        )
                        model.users
              }
            , NoOutMsg
            )


updateWorldUpdateDurations :
    Duration
    -> { a | worldUpdateDurations : Array Duration }
    -> { a | worldUpdateDurations : Array Duration }
updateWorldUpdateDurations duration model =
    let
        newArray =
            Array.push duration model.worldUpdateDurations

        maxSize =
            1000
    in
    { model
        | worldUpdateDurations =
            if Array.length model.worldUpdateDurations > maxSize then
                newArray

            else
                Array.slice (Array.length newArray - maxSize) (Array.length newArray) newArray
    }


addReported :
    Id UserId
    -> BackendReport
    -> IdDict UserId (Nonempty BackendReport)
    -> IdDict UserId (Nonempty BackendReport)
addReported userId report reported =
    IdDict.update
        userId
        (\maybeList ->
            (case maybeList of
                Just nonempty ->
                    List.Nonempty.cons report nonempty

                Nothing ->
                    List.Nonempty.singleton report
            )
                |> Just
        )
        reported


removeReported :
    Id UserId
    -> Coord WorldUnit
    -> IdDict UserId (Nonempty { a | position : Coord WorldUnit })
    -> IdDict UserId (Nonempty { a | position : Coord WorldUnit })
removeReported userId position reported =
    IdDict.update
        userId
        (\maybeList ->
            case maybeList of
                Just nonempty ->
                    List.Nonempty.toList nonempty
                        |> List.filter (\report -> report.position /= position)
                        |> List.Nonempty.fromList

                Nothing ->
                    Nothing
        )
        reported


moveOutOfCollision :
    Point2d WorldUnit WorldUnit
    -> List (BoundingBox2d WorldUnit WorldUnit)
    -> Point2d WorldUnit WorldUnit
moveOutOfCollision position bounds =
    List.map
        (\boundingBox -> BoundingBox2d.extrema boundingBox |> .maxY |> Point2d.xy (Point2d.xCoordinate position))
        bounds
        |> Quantity.maximumBy Point2d.yCoordinate
        |> Maybe.withDefault position


update_ : Change -> LocalGrid_ -> ( LocalGrid_, OutMsg )
update_ msg model =
    case msg of
        LocalChange _ localChange ->
            updateLocalChange localChange model

        ServerChange serverChange ->
            updateServerChange serverChange model


config : LocalModel.Config Change LocalGrid OutMsg
config =
    { msgEqual =
        \msg0 msg1 ->
            case ( msg0, msg1 ) of
                ( LocalChange eventId0 _, LocalChange eventId1 _ ) ->
                    eventId0 == eventId1

                _ ->
                    msg0 == msg1
    , update = \msg (LocalGrid model) -> update_ msg model |> Tuple.mapFirst LocalGrid
    }


randomAnimalsHelper : Coord WorldUnit -> List Animal -> List AnimalType -> Random.Generator (List Animal)
randomAnimalsHelper worldCoord output list =
    case list of
        head :: rest ->
            randomAnimal head worldCoord
                |> Random.andThen (\animal -> randomAnimalsHelper worldCoord (animal :: output) rest)

        [] ->
            Random.constant output


randomAnimal : AnimalType -> Coord WorldUnit -> Random.Generator Animal
randomAnimal animalType ( Quantity xOffset, Quantity yOffset ) =
    Random.map2
        (\x y ->
            let
                position =
                    Point2d.unsafe { x = toFloat xOffset + x, y = toFloat yOffset + y }
            in
            { position = position
            , endPosition = position
            , startTime = Effect.Time.millisToPosix 0
            , animalType = animalType
            }
        )
        (Random.float 0 Units.cellSize)
        (Random.float 0 Units.cellSize)
