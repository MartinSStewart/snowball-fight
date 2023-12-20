module Shaders exposing
    ( InstancedVertex
    , RenderData
    , ScissorBox
    , blend
    , clearDepth
    , depthTest
    , drawBackground
    , fragmentShader
    , instancedVertexShader
    , noUserIdSelected
    , opacityAndUserId
    , scissorBox
    , triangleFan
    , vertexShader
    )

import Bitwise
import Coord exposing (Coord)
import Dict exposing (Dict)
import Effect.WebGL exposing (Shader)
import Effect.WebGL.Settings exposing (Setting)
import Effect.WebGL.Settings.Blend as Blend
import Effect.WebGL.Settings.DepthTest
import Id exposing (Id, UserId)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Sprite exposing (Vertex)
import WebGL.Texture


type alias InstancedVertex =
    { localPosition : Vec2
    , index : Float
    }


type alias RenderData =
    { nightFactor : Float
    , texture : WebGL.Texture.Texture
    , depth : WebGL.Texture.Texture
    , staticViewMatrix : Mat4
    , viewMatrix : Mat4
    , time : Float
    , scissors : ScissorBox
    }


noUserIdSelected : number
noUserIdSelected =
    -2


opacityAndUserId : Float -> Id UserId -> Float
opacityAndUserId opacity userId =
    opacity
        * Sprite.opaque
        |> round
        |> toFloat
        |> (+) (Id.toInt userId |> Bitwise.shiftLeftBy 4 |> toFloat)


triangleFan : List attributes -> Effect.WebGL.Mesh attributes
triangleFan vertices =
    --let
    --    _ =
    --        Debug.log "new triangleFan" ""
    --in
    Effect.WebGL.triangleFan vertices


blend : Setting
blend =
    Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha


clearDepth : Float -> Vec4 -> ScissorBox -> Effect.WebGL.Entity
clearDepth nightFactor color scissors =
    Effect.WebGL.entityWith
        [ Effect.WebGL.Settings.DepthTest.always { write = True, near = 0, far = 1 }
        , blend
        , scissorBox scissors
        ]
        fillVertexShader
        fillFragmentShader
        viewportSquare
        { color = color, night = nightFactor }


viewportSquare : Effect.WebGL.Mesh { x : Float, y : Float }
viewportSquare =
    Effect.WebGL.triangleFan
        [ { x = -1, y = -1 }
        , { x = 1, y = -1 }
        , { x = 1, y = 1 }
        , { x = -1, y = 1 }
        ]


fillVertexShader : Shader { x : Float, y : Float } u {}
fillVertexShader =
    [glsl|

attribute float x;
attribute float y;

void main () {
  gl_Position = vec4(vec2(x, y), 1.0, 1.0);
}

|]


fillFragmentShader : Shader {} { u | color : Vec4, night : Float } {}
fillFragmentShader =
    [glsl|
precision mediump float;
uniform vec4 color;
uniform float night;

void main () {
    vec3 nightColor = vec3(1.0, 1.0, 1.0) * (1.0 - night) + vec3(0.33, 0.4, 0.645) * night;
    
    gl_FragColor = color * vec4(nightColor, 1.0);
}
    |]


drawBackground :
    RenderData
    -> Dict ( Int, Int ) { foreground : Effect.WebGL.Mesh Vertex, background : Effect.WebGL.Mesh Vertex }
    -> List Effect.WebGL.Entity
drawBackground { nightFactor, viewMatrix, texture, depth, time, scissors } meshes =
    Dict.toList meshes
        |> List.map
            (\( _, mesh ) ->
                Effect.WebGL.entityWith
                    [ Effect.WebGL.Settings.cullFace Effect.WebGL.Settings.back
                    , Effect.WebGL.Settings.DepthTest.default
                    , blend
                    , scissorBox scissors
                    ]
                    vertexShader
                    fragmentShader
                    mesh.background
                    { view = viewMatrix
                    , texture = texture
                    , textureSize = WebGL.Texture.size texture |> Coord.tuple |> Coord.toVec2
                    , color = Vec4.vec4 1 1 1 1
                    , userId = noUserIdSelected
                    , time = time
                    , night = nightFactor
                    , depth = depth
                    , waterReflection = 0
                    }
            )


depthTest : Setting
depthTest =
    Effect.WebGL.Settings.DepthTest.lessOrEqual { write = True, near = 0, far = 1 }


type alias ScissorBox =
    { left : Int, bottom : Int, width : Int, height : Int }


scissorBox : ScissorBox -> Setting
scissorBox { left, bottom, width, height } =
    Effect.WebGL.Settings.scissor left bottom width height


vertexShader :
    Shader
        Vertex
        { u | view : Mat4, textureSize : Vec2 }
        { vcoord : Vec2
        , opacity : Float
        , primaryColor2 : Vec3
        , secondaryColor2 : Vec3
        , position2 : Vec2
        , z2 : Float
        , textureSize2 : Vec2
        }
vertexShader =
    [glsl|
attribute float x;
attribute float y;
attribute float z;
attribute float texturePosition;
attribute float opacityAndUserId;
attribute float primaryColor;
attribute float secondaryColor;
uniform mat4 view;
uniform vec2 textureSize;
varying vec2 vcoord;
varying float opacity;
varying vec3 primaryColor2;
varying vec3 secondaryColor2;
varying vec2 position2;
varying float z2;
varying vec2 textureSize2;

int OR(int n1, int n2){

    float v1 = float(n1);
    float v2 = float(n2);

    int byteVal = 1;
    int result = 0;

    for(int i = 0; i < 32; i++){
        bool keepGoing = v1>0.0 || v2 > 0.0;
        if(keepGoing){

            bool addOn = mod(v1, 2.0) > 0.0 || mod(v2, 2.0) > 0.0;

            if(addOn){
                result += byteVal;
            }

            v1 = floor(v1 / 2.0);
            v2 = floor(v2 / 2.0);

            byteVal *= 2;
        } else {
            return result;
        }
    }
    return result;
}

int AND(int n1, int n2){

    float v1 = float(n1);
    float v2 = float(n2);

    int byteVal = 1;
    int result = 0;

    for(int i = 0; i < 32; i++){
        bool keepGoing = v1>0.0 || v2 > 0.0;
        if(keepGoing){

            bool addOn = mod(v1, 2.0) > 0.0 && mod(v2, 2.0) > 0.0;

            if(addOn){
                result += byteVal;
            }

            v1 = floor(v1 / 2.0);
            v2 = floor(v2 / 2.0);
            byteVal *= 2;
        } else {
            return result;
        }
    }
    return result;
}

int RShift(int num, float shifts){
    return int(floor(float(num) / pow(2.0, shifts)));
}

vec3 floatColorToVec3(float color) {
    int colorInt = int(color);
    float blue = float(AND(colorInt, 0xFF)) / 255.0;
    float green = float(AND(RShift(colorInt, 8.0), 0xFF)) / 255.0;
    float red = float(AND(RShift(colorInt, 16.0), 0xFF)) / 255.0;
    return vec3(red, green, blue);
}

void main () {
    gl_Position = view * vec4(vec3(x, y, z), 1.0);

    float y2 = floor(texturePosition / textureSize.x);
    vcoord = vec2(texturePosition - y2 * textureSize.x, y2) / textureSize;
    opacity = float(AND(int(opacityAndUserId), 0xF)) / 15.0;

    primaryColor2 = floatColorToVec3(primaryColor);
    secondaryColor2 = floatColorToVec3(secondaryColor);
    position2 = vec2(x, y);
    z2 = z;
    textureSize2 = textureSize;
}|]


instancedVertexShader :
    Shader
        InstancedVertex
        { u
            | view : Mat4
            , textureSize : Vec2
            , userId : Float
            , position0 : Vec3
            , size0 : Vec2
            , texturePosition0 : Float
            , opacityAndUserId0 : Float
            , primaryColor0 : Float
            , secondaryColor0 : Float
        }
        { vcoord : Vec2
        , opacity : Float
        , primaryColor2 : Vec3
        , secondaryColor2 : Vec3
        , isSelected : Float
        , position2 : Vec2
        , z2 : Float
        , textureSize2 : Vec2
        }
instancedVertexShader =
    [glsl|
attribute vec2 localPosition;
attribute float index;
uniform mat4 view;
uniform vec2 textureSize;
uniform float userId;
uniform vec3 position0;
uniform vec2 size0;
uniform float texturePosition0;
uniform float opacityAndUserId0;
uniform float primaryColor0;
uniform float secondaryColor0;
varying vec2 vcoord;
varying float opacity;
varying vec3 primaryColor2;
varying vec3 secondaryColor2;
varying float isSelected;
varying vec2 position2;
varying float z2;
varying vec2 textureSize2;

int OR(int n1, int n2){

    float v1 = float(n1);
    float v2 = float(n2);

    int byteVal = 1;
    int result = 0;

    for(int i = 0; i < 32; i++){
        bool keepGoing = v1>0.0 || v2 > 0.0;
        if(keepGoing){

            bool addOn = mod(v1, 2.0) > 0.0 || mod(v2, 2.0) > 0.0;

            if(addOn){
                result += byteVal;
            }

            v1 = floor(v1 / 2.0);
            v2 = floor(v2 / 2.0);

            byteVal *= 2;
        } else {
            return result;
        }
    }
    return result;
}

int AND(int n1, int n2){

    float v1 = float(n1);
    float v2 = float(n2);

    int byteVal = 1;
    int result = 0;

    for(int i = 0; i < 32; i++){
        bool keepGoing = v1>0.0 || v2 > 0.0;
        if(keepGoing){

            bool addOn = mod(v1, 2.0) > 0.0 && mod(v2, 2.0) > 0.0;

            if(addOn){
                result += byteVal;
            }

            v1 = floor(v1 / 2.0);
            v2 = floor(v2 / 2.0);
            byteVal *= 2;
        } else {
            return result;
        }
    }
    return result;
}

int RShift(int num, float shifts){
    return int(floor(float(num) / pow(2.0, shifts)));
}

vec3 floatColorToVec3(float color) {
    int colorInt = int(color);
    float blue = float(AND(colorInt, 0xFF)) / 255.0;
    float green = float(AND(RShift(colorInt, 8.0), 0xFF)) / 255.0;
    float red = float(AND(RShift(colorInt, 16.0), 0xFF)) / 255.0;
    return vec3(red, green, blue);
}

void main () {
    vec2 localPosition2 = localPosition * size0;

    gl_Position = view * vec4(position0 + vec3(localPosition2.xy, 0.0), 1.0);



    float y2 = floor(texturePosition0 / textureSize.x);
    vcoord = (vec2(texturePosition0 - y2 * textureSize.x, y2) + localPosition2) / textureSize;
    opacity = float(AND(int(opacityAndUserId0), 0xF)) / 15.0;
    isSelected = userId == float(RShift(int(opacityAndUserId0), 4.0)) ? 1.0 : 0.0;


    primaryColor2 = floatColorToVec3(primaryColor0);
    secondaryColor2 = floatColorToVec3(secondaryColor0);
    position2 = position0.xy + localPosition2;
    z2 = 0.0;
    textureSize2 = textureSize;
}|]


fragmentShader :
    Shader
        {}
        { u
            | texture : WebGL.Texture.Texture
            , textureSize : Vec2
            , depth : WebGL.Texture.Texture
            , time : Float
            , color : Vec4
            , night : Float
        }
        { vcoord : Vec2
        , opacity : Float
        , primaryColor2 : Vec3
        , secondaryColor2 : Vec3
        , position2 : Vec2
        , z2 : Float
        , textureSize2 : Vec2
        }
fragmentShader =
    [glsl|
precision mediump float;
uniform sampler2D texture;
uniform sampler2D depth;
uniform float time;
uniform vec4 color;
uniform float night;
varying vec2 vcoord;
varying float opacity;
varying vec3 primaryColor2;
varying vec3 secondaryColor2;
varying vec2 position2;
varying float z2;
varying vec2 textureSize2;

vec3 primaryColor = vec3(1.0, 0.0, 1.0);
vec3 primaryColorMidShade = vec3(233.0 / 255.0, 45.0 / 255.0, 231.0 / 255.0);
vec3 primaryColorShade = vec3(209.0 / 255.0, 64.0 / 255.0, 206.0 / 255.0);
vec3 secondaryColor = vec3(0.0, 1.0, 1.0);
vec3 secondaryColorMidShade = vec3(0.0 / 255.0, 229.0 / 255.0, 229.0 / 255.0);
vec3 secondaryColorShade = vec3(96.0 / 255.0, 209.0 / 255.0, 209.0 / 255.0);

void main () {
    vec2 vcoord2 = vcoord;

    vec4 textureColor = texture2D(texture, vcoord2);

    if (textureColor.a == 0.0) {
        discard;
    }

    vec4 textureColor2 =
        textureColor.xyz == primaryColor
            ? vec4(primaryColor2, opacity)
            : textureColor.xyz == primaryColorMidShade
                ? vec4(primaryColor2 * 0.9, opacity)
                : textureColor.xyz == primaryColorShade
                    ? vec4(primaryColor2 * 0.8, opacity)
                    : textureColor.xyz == secondaryColor
                        ? vec4(secondaryColor2, opacity)
                        : textureColor.xyz == secondaryColorMidShade
                            ? vec4(secondaryColor2 * 0.9, opacity)
                            : textureColor.xyz == secondaryColorShade
                                ? vec4(secondaryColor2 * 0.8, opacity)
                                : vec4(textureColor.xyz, opacity);

    gl_FragColor = textureColor2 * color;
}|]
