port module Ports exposing (audioPortFromJS, audioPortToJS, getDevicePixelRatio, gotDevicePixelRatio, mouse_leave)

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


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


port mouse_leave : (Json.Decode.Value -> msg) -> Sub msg


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
