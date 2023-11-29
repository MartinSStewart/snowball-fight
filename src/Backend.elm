module Backend exposing
    ( app
    , app_
    )

import Array exposing (Array)
import AssocSet
import Bytes exposing (Endianness(..))
import Bytes.Decode
import Change exposing (AdminChange(..), AdminData, AreTrainsAndAnimalsDisabled(..), LocalChange(..), ServerChange(..), UserStatus(..), ViewBoundsChange2)
import Crypto.Hash
import Dict
import Duration exposing (Duration)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Env
import Id exposing (AnimalId, EventId, Id, MailId, OneTimePasswordId, SecretId, TrainId, UserId)
import Lamdera
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Quantity
import SHA224
import Types exposing (BackendModel, BackendMsg(..), LoadingData_, ToBackend(..), ToFrontend(..))


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : String -> String -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        (app_ Env.isProduction)


app_ :
    Bool
    ->
        { init : ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
        , update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
        , updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
        , subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
        }
app_ isProduction =
    { init = init
    , update = update isProduction
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


updateFromFrontend :
    SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    ( model, Effect.Time.now |> Effect.Task.perform (UpdateFromFrontend sessionId clientId msg) )


subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
subscriptions model =
    Subscription.batch
        [ Effect.Lamdera.onDisconnect UserDisconnected
        , Effect.Lamdera.onConnect (\_ clientId -> UserConnected clientId)
        ]


init : ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { userSessions = Dict.empty, userIdCounter = 0 }, Command.none )


adminId : Id UserId
adminId =
    Id.fromInt 0


disconnectClient : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend backendMsg )
disconnectClient sessionId clientId model =
    case Dict.get (Effect.Lamdera.sessionIdToString sessionId) model.userSessions of
        Just session ->
            ( { model
                | userSessions =
                    Dict.insert
                        (Effect.Lamdera.sessionIdToString sessionId)
                        { clientIds = AssocSet.remove clientId session.clientIds
                        , userId = session.userId
                        }
                        model.userSessions
              }
            , Nonempty (ServerUserDisconnected session.userId |> Change.ServerChange) []
                |> ChangeBroadcast
                |> Effect.Lamdera.broadcast
            )

        Nothing ->
            ( model, Command.none )


update : Bool -> BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update isProduction msg model =
    case msg of
        UserDisconnected sessionId clientId ->
            disconnectClient sessionId clientId model

        UserConnected clientId ->
            ( model, Effect.Lamdera.sendToFrontend clientId ClientConnected )

        UpdateFromFrontend sessionId clientId toBackend time ->
            updateFromFrontendWithTime isProduction time sessionId clientId toBackend model


localChangeStatusToLocalChange : Id EventId -> LocalChange -> LocalChangeStatus -> Change.Change
localChangeStatusToLocalChange eventId originalChange localChangeStatus =
    Change.LocalChange
        eventId
        (case localChangeStatus of
            OriginalChange ->
                originalChange

            InvalidChange ->
                Change.InvalidChange

            NewLocalChange localChange ->
                localChange
        )


broadcastLocalChange :
    Bool
    -> Effect.Time.Posix
    -> SessionId
    -> ClientId
    -> Nonempty ( Id EventId, Change.LocalChange )
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
broadcastLocalChange isProduction time sessionId clientId changes model =
    let
        ( model2, localChange, firstMsg ) =
            updateLocalChange sessionId clientId time (Nonempty.head changes |> Tuple.second) model

        ( model3, allLocalChanges, serverChanges ) =
            Nonempty.tail changes
                |> List.foldl
                    (\( eventId2, change ) ( model_, originalChanges, serverChanges_ ) ->
                        let
                            ( newModel, localChange2, serverChange_ ) =
                                updateLocalChange sessionId clientId time change model_
                        in
                        ( newModel
                        , Nonempty.cons (localChangeStatusToLocalChange eventId2 change localChange2) originalChanges
                        , Nonempty.cons serverChange_ serverChanges_
                        )
                    )
                    ( model2
                    , Nonempty.singleton
                        (localChangeStatusToLocalChange
                            (Nonempty.head changes |> Tuple.first)
                            (Nonempty.head changes |> Tuple.second)
                            localChange
                        )
                    , Nonempty.singleton firstMsg
                    )
                |> (\( a, b, c ) -> ( a, Nonempty.reverse b, Nonempty.reverse c ))
    in
    ( model3
    , Command.batch
        [ broadcast
            (\sessionId_ clientId_ ->
                if clientId == clientId_ then
                    ChangeBroadcast allLocalChanges |> Just

                else
                    Nonempty.toList serverChanges
                        |> List.filterMap
                            (\broadcastTo ->
                                case broadcastTo of
                                    BroadcastToEveryoneElse serverChange ->
                                        Change.ServerChange serverChange |> Just

                                    BroadcastToNoOne ->
                                        Nothing

                                    BroadcastToRestOfSessionAndEveryoneElse sessionId2 restOfSession everyoneElse ->
                                        (if sessionId_ == sessionId2 then
                                            restOfSession

                                         else
                                            everyoneElse
                                        )
                                            |> Change.ServerChange
                                            |> Just
                            )
                        |> Nonempty.fromList
                        |> Maybe.map ChangeBroadcast
            )
            model3
        , Command.none
        ]
    )


generateSecretId : Effect.Time.Posix -> { a | secretLinkCounter : Int } -> ( SecretId b, { a | secretLinkCounter : Int } )
generateSecretId currentTime model =
    ( Env.secretKey
        ++ "_"
        ++ String.fromInt (Effect.Time.posixToMillis currentTime)
        ++ "_"
        ++ String.fromInt model.secretLinkCounter
        |> Crypto.Hash.sha256
        |> Id.secretFromString
    , { model | secretLinkCounter = model.secretLinkCounter + 1 }
    )


generateOneTimePassword :
    Effect.Time.Posix
    -> { a | secretLinkCounter : Int }
    -> ( SecretId OneTimePasswordId, { a | secretLinkCounter : Int } )
generateOneTimePassword currentTime model =
    ( Env.secretKey
        ++ "_"
        ++ String.fromInt (Effect.Time.posixToMillis currentTime)
        ++ "_"
        ++ String.fromInt model.secretLinkCounter
        |> SHA224.fromString
        |> SHA224.toBytes
        |> Bytes.Decode.decode oneTimePasswordDecoder
        |> Maybe.withDefault ""
        |> Id.secretFromString
    , { model | secretLinkCounter = model.secretLinkCounter + 1 }
    )


oneTimePasswordDecoder : Bytes.Decode.Decoder String
oneTimePasswordDecoder =
    Bytes.Decode.loop
        ( [], 0 )
        (\( list, count ) ->
            if count >= Id.oneTimePasswordLength then
                Bytes.Decode.succeed (Bytes.Decode.Done (String.fromList list))

            else
                Bytes.Decode.map
                    (\value ->
                        ( (Array.get
                            (modBy (Array.length oneTimePasswordChars - 1) value)
                            oneTimePasswordChars
                            |> Maybe.withDefault '?'
                          )
                            :: list
                        , count + 1
                        )
                            |> Bytes.Decode.Loop
                    )
                    (Bytes.Decode.unsignedInt16 BE)
        )


oneTimePasswordChars : Array Char
oneTimePasswordChars =
    List.range (Char.toCode 'a') (Char.toCode 'z')
        ++ List.range (Char.toCode 'A') (Char.toCode 'Z')
        ++ List.range (Char.toCode '0') (Char.toCode '9')
        |> List.map Char.fromCode
        -- Remove chars that are easily confused with eachother
        |> List.remove 'O'
        |> List.remove '0'
        |> List.remove 'l'
        |> List.remove '1'
        |> Array.fromList


updateFromFrontendWithTime :
    Bool
    -> Effect.Time.Posix
    -> SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontendWithTime isProduction currentTime sessionId clientId msg model =
    case msg of
        ConnectToBackend ->
            addSession sessionId clientId model

        PingRequest ->
            ( model, PingResponse currentTime |> Effect.Lamdera.sendToFrontend clientId )


{-| Allow a client to say when something happened but restrict how far it can be away from the current time.
-}
adjustEventTime : Effect.Time.Posix -> Effect.Time.Posix -> Effect.Time.Posix
adjustEventTime currentTime eventTime =
    if Duration.from currentTime eventTime |> Quantity.abs |> Quantity.lessThan (Duration.seconds 1) then
        eventTime

    else
        currentTime


type BroadcastTo
    = BroadcastToEveryoneElse ServerChange
    | BroadcastToNoOne
    | BroadcastToRestOfSessionAndEveryoneElse SessionId ServerChange ServerChange


type LocalChangeStatus
    = OriginalChange
    | InvalidChange
    | NewLocalChange LocalChange


updateLocalChange :
    SessionId
    -> ClientId
    -> Effect.Time.Posix
    -> Change.LocalChange
    -> BackendModel
    -> ( BackendModel, LocalChangeStatus, BroadcastTo )
updateLocalChange sessionId clientId time change model =
    case change of
        Change.SelectCharacter ->
            ( model, OriginalChange, BroadcastToNoOne )

        Change.InvalidChange ->
            ( model, InvalidChange, BroadcastToNoOne )


addSession :
    SessionId
    -> ClientId
    -> BackendModel
    -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
addSession sessionId clientId model =
    ( { model
        | userSessions =
            Dict.update
                (Effect.Lamdera.sessionIdToString sessionId)
                (\maybeSession ->
                    (case maybeSession of
                        Just session ->
                            { clientIds = AssocSet.insert clientId session.clientIds
                            , userId = session.userId
                            }

                        Nothing ->
                            { clientIds = AssocSet.singleton clientId
                            , userId = Id.fromInt model.userIdCounter
                            }
                    )
                        |> Just
                )
                model.userSessions
        , userIdCounter = model.userIdCounter + 1
      }
    , LoadingData
        { users =
            Dict.toList model.userSessions
                |> List.map (\( _, session ) -> session.userId)
        }
        |> Effect.Lamdera.sendToFrontend clientId
    )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> Command BackendOnly ToFrontend BackendMsg
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap
            (\( sessionId, { clientIds } ) ->
                AssocSet.toList clientIds |> List.map (Tuple.pair (Effect.Lamdera.sessionIdFromString sessionId))
            )
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (Effect.Lamdera.sendToFrontend clientId))
        |> Command.batch
