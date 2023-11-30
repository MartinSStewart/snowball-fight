port module Ports exposing (audioPortFromJS, audioPortToJS, copyToClipboard, getDevicePixelRatio, getLocalStorage, gotDevicePixelRatio, gotWebGlFix, mouse_leave, readFromClipboardRequest, readFromClipboardResponse, user_agent_from_js, user_agent_to_js, webGlFix)

import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Subscription as Subscription exposing (Subscription)
import Hyperlink exposing (Hyperlink)
import Json.Decode
import Json.Encode
import Serialize exposing (Codec)
import Sound
import Types exposing (UserSettings)


port martinsstewart_elm_device_pixel_ratio_from_js : (Json.Decode.Value -> msg) -> Sub msg


port martinsstewart_elm_device_pixel_ratio_to_js : Json.Encode.Value -> Cmd msg


port user_agent_to_js : Json.Encode.Value -> Cmd msg


port user_agent_from_js : (Json.Decode.Value -> msg) -> Sub msg


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


port supermario_copy_to_clipboard_to_js : Json.Encode.Value -> Cmd msg


port mouse_leave : (Json.Decode.Value -> msg) -> Sub msg


port get_local_storage : Json.Encode.Value -> Cmd msg


port webgl_fix_to_js : Json.Encode.Value -> Cmd msg


port webgl_fix_from_js : (Json.Decode.Value -> msg) -> Sub msg


getDevicePixelRatio : Command FrontendOnly toMsg msg
getDevicePixelRatio =
    Command.sendToJs "martinsstewart_elm_device_pixel_ratio_to_js" martinsstewart_elm_device_pixel_ratio_to_js Json.Encode.null


gotDevicePixelRatio : (Float -> msg) -> Subscription.Subscription FrontendOnly msg
gotDevicePixelRatio msg =
    Subscription.fromJs
        "martinsstewart_elm_device_pixel_ratio_from_js"
        martinsstewart_elm_device_pixel_ratio_from_js
        (\value ->
            Json.Decode.decodeValue Json.Decode.float value
                |> Result.withDefault 1
                |> msg
        )


webGlFix : Command FrontendOnly toMsg msg
webGlFix =
    Command.sendToJs
        "webgl_fix_to_js"
        webgl_fix_to_js
        Json.Encode.null


gotWebGlFix : msg -> Subscription FrontendOnly msg
gotWebGlFix sub =
    Subscription.fromJs
        "webgl_fix_from_js"
        webgl_fix_from_js
        (\_ -> sub)


getLocalStorage : Command FrontendOnly toMsg msg
getLocalStorage =
    Command.sendToJs
        "get_local_storage"
        get_local_storage
        Json.Encode.null


userSettingsCodec : Codec e UserSettings
userSettingsCodec =
    Serialize.record UserSettings
        |> Serialize.field .musicVolume Serialize.byte
        |> Serialize.field .soundEffectVolume Serialize.byte
        |> Serialize.finishRecord


copyToClipboard : String -> Command FrontendOnly toMsg msg
copyToClipboard text =
    Command.sendToJs
        "supermario_copy_to_clipboard_to_js"
        supermario_copy_to_clipboard_to_js
        (Json.Encode.string text)


port supermario_read_from_clipboard_to_js : Json.Encode.Value -> Cmd msg


readFromClipboardRequest : Command FrontendOnly toMsg msg
readFromClipboardRequest =
    Command.sendToJs
        "supermario_read_from_clipboard_to_js"
        supermario_read_from_clipboard_to_js
        Json.Encode.null


port supermario_read_from_clipboard_from_js : (Json.Decode.Value -> msg) -> Sub msg


readFromClipboardResponse : (String -> msg) -> Subscription FrontendOnly msg
readFromClipboardResponse msg =
    Subscription.fromJs
        "supermario_read_from_clipboard_from_js"
        supermario_read_from_clipboard_from_js
        (\value ->
            Json.Decode.decodeValue Json.Decode.string value
                |> Result.withDefault ""
                |> msg
        )
