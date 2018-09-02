module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Random
import Task
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    , world : Physics.World
    , devicePixelRatio : Float
    , texture : Maybe Texture
    }


type Msg
    = Tick Float
    | Resize Float Float
    | Restart
    | NewWorld Physics.World
    | TextureLoaded (Result Error Texture)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


box : Physics.Body
box =
    Physics.body
        |> Physics.setMass 5
        |> Physics.addShape (Physics.box (vec3 1 1 1))
        |> Tuple.first


plane : Physics.Body
plane =
    Physics.body
        |> Physics.offsetBy (vec3 0 0 -3)
        |> Physics.addShape Physics.plane
        |> Tuple.first


{-| Drop bodyId from (World, BodyId) tuple
-}
addBody : Physics.Body -> Physics.World -> Physics.World
addBody body =
    Physics.addBody body >> Tuple.first


randomWorld : Random.Generator Physics.World
randomWorld =
    Random.map2
        (\b1 b2 ->
            initialSetup
                |> Tuple.first
                |> addBody (Physics.offsetBy (vec3 -0.5 0 3) b1)
                |> addBody (Physics.offsetBy (vec3 0.5 0 0) b2)
        )
        randomBox
        randomBox


randomBox : Random.Generator Physics.Body
randomBox =
    Random.map4
        (\a x y z ->
            box
                |> Physics.offsetBy (vec3 0 0 10)
                |> Physics.rotateBy (Vec3.normalize (vec3 x y z)) a
        )
        (Random.float -pi pi)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


{-| A pair of initial world and the last plane id
-}
initialSetup : ( Physics.World, Physics.BodyId )
initialSetup =
    Physics.world
        |> Physics.setGravity (vec3 0 0 -10)
        |> addBody plane
        |> addBody
            (plane
                |> Physics.offsetBy (vec3 -4 0 0)
                |> Physics.rotateBy Vec3.j (pi / 3)
            )
        |> addBody
            (plane
                |> Physics.offsetBy (vec3 4 0 0)
                |> Physics.rotateBy Vec3.j -(pi / 3)
            )
        |> addBody
            (plane
                |> Physics.offsetBy (vec3 0 -4 0)
                |> Physics.rotateBy Vec3.i -(pi / 3)
            )
        |> Physics.addBody
            (plane
                |> Physics.offsetBy (vec3 0 4 0)
                |> Physics.rotateBy Vec3.i (pi / 3)
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenWidth = 1
      , screenHeight = 1
      , world = Tuple.first initialSetup
      , devicePixelRatio = 2
      , texture = Nothing
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize viewport.width viewport.height
            )
            getViewport
        , Random.generate NewWorld randomWorld
        , "data:image/png;base64,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"
            |> Texture.load
            |> Task.attempt TextureLoaded
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        Resize width height ->
            ( { model
                | screenWidth = width
                , screenHeight = height
              }
            , Cmd.none
            )

        Restart ->
            ( model, Random.generate NewWorld randomWorld )

        NewWorld world ->
            ( { model | world = world }
            , Cmd.none
            )

        Tick dt ->
            ( { model | world = Physics.step (1 / 60) model.world }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> Resize (toFloat w) (toFloat h))
        , onAnimationFrameDelta Tick
        ]



-- View:


view : Model -> Html Msg
view { screenWidth, screenHeight, devicePixelRatio, world, texture } =
    WebGL.toHtml
        [ width (round (screenWidth * devicePixelRatio))
        , height (round (screenHeight * devicePixelRatio))
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , style "width" (String.fromFloat screenWidth ++ "px")
        , style "height" (String.fromFloat screenHeight ++ "px")
        , onClick Restart
        ]
        (let
            camera =
                Mat4.makeLookAt (Vec3.vec3 0 0.1 25) (Vec3.vec3 0 0 0) Vec3.k

            perspective =
                Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000
         in
         texture
            |> Maybe.map
                (\text ->
                    Physics.fold (addEntity camera perspective text) [] world
                )
            |> Maybe.withDefault []
        )


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    , textureNumber : Float
    , texturePosition : Vec3
    }


type alias Uniforms =
    { camera : Mat4
    , perspective : Mat4
    , transform : Mat4
    , texture : Texture
    }


type alias Varying =
    { vTextureNumber : Float
    , vTexturePosition : Vec3
    , vlighting : Float
    }


addEntity : Mat4 -> Mat4 -> Texture -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addEntity camera perspective texture { transform, bodyId } =
    (::)
        (WebGL.entity
            vertex
            fragment
            (if bodyId <= Tuple.second initialSetup then
                planeMesh

             else
                cubeMesh
            )
            { transform = transform
            , perspective = perspective
            , camera = camera
            , texture = texture
            }
        )



-- Meshes:


planeMesh : Mesh Attributes
planeMesh =
    face
        (vec3 10 10 0)
        (vec3 -10 10 0)
        (vec3 -10 -10 0)
        (vec3 10 -10 0)
        6
        |> WebGL.triangles


cubeMesh : Mesh Attributes
cubeMesh =
    let
        v0 =
            vec3 -1 -1 -1

        v1 =
            vec3 1 -1 -1

        v2 =
            vec3 1 1 -1

        v3 =
            vec3 -1 1 -1

        v4 =
            vec3 -1 -1 1

        v5 =
            vec3 1 -1 1

        v6 =
            vec3 1 1 1

        v7 =
            vec3 -1 1 1
    in
    [ face v3 v2 v1 v0 0
    , face v4 v5 v6 v7 1
    , face v5 v4 v0 v1 2
    , face v2 v3 v7 v6 3
    , face v0 v4 v7 v3 4
    , face v1 v2 v6 v5 5
    ]
        |> List.concat
        |> WebGL.triangles


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> List ( Attributes, Attributes, Attributes )
face a b c d number =
    let
        normal =
            Vec3.cross (Vec3.sub a b) (Vec3.sub a c)
    in
    [ ( Attributes a normal number (vec3 0 0 0)
      , Attributes b normal number (vec3 1 0 0)
      , Attributes c normal number (vec3 1 1 0)
      )
    , ( Attributes c normal number (vec3 1 1 0)
      , Attributes d normal number (vec3 0 1 0)
      , Attributes a normal number (vec3 0 0 0)
      )
    ]



-- Shaders:


vertex : Shader Attributes Uniforms Varying
vertex =
    [glsl|
        precision mediump float;
        attribute vec3 position;
        attribute vec3 texturePosition;
        attribute float textureNumber;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        uniform sampler2D texture;
        varying vec3 vTexturePosition;
        varying float vTextureNumber;
        varying float vlighting;
        void main () {
          float ambientLight = 0.3;
          float directionalLight = 0.7;
          vec3 directionalVector = normalize(vec3(0.3, 0.1, 1.0));
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          vec4 transformedNormal = normalize(transform * vec4(normal, 0.0));
          float directional = max(dot(transformedNormal.xyz, directionalVector), 0.0);
          vlighting = ambientLight + directional * directionalLight;
          vTextureNumber = textureNumber;
          vTexturePosition = texturePosition;
        }
    |]


fragment : Shader {} Uniforms Varying
fragment =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec3 vTexturePosition;
        varying float vTextureNumber;
        varying float vlighting;
        void main () {
          if (ceil(vTextureNumber) < 6.0) {
            gl_FragColor = vec4(vlighting * vec3(texture2D(texture, vec2((vTexturePosition.x + vTextureNumber) / 8.0, vTexturePosition.y))), 1.0);
          } else {
            gl_FragColor = vec4(vlighting * vec3(0.3, 0.3, 0.3), 1.0);
          }
        }
    |]
