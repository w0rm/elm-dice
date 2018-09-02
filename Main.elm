module Dice exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Physics
import Random
import Task
import Time exposing (Time)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)
import Window


type alias Model =
    { screenWidth : Float
    , screenHeight : Float
    , world : Physics.World
    , devicePixelRatio : Float
    , texture : Maybe Texture
    }


type Msg
    = Tick Time
    | Resize Window.Size
    | Restart
    | NewWorld Physics.World
    | TextureLoaded (Result Error Texture)


main : Program Never Model Msg
main =
    Html.program
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


randomWorld : Random.Generator Physics.World
randomWorld =
    Random.map2
        (\b1 b2 ->
            newWorld
                |> Physics.addBody (Physics.offsetBy (vec3 -0.5 0 3) b1)
                |> Physics.addBody (Physics.offsetBy (vec3 0.5 0 0) b2)
        )
        randomBox
        randomBox


randomBox : Random.Generator Physics.Body
randomBox =
    Random.map4
        (\angle x y z ->
            box
                |> Physics.offsetBy (vec3 0 0 10)
                |> Physics.rotateBy (Vec3.normalize (vec3 x y z)) angle
        )
        (Random.float -pi pi)
        (Random.float -1 1)
        (Random.float -1 1)
        (Random.float -1 1)


delta : Float
delta =
    4


angle : Float
angle =
    pi / 3


newWorld : Physics.World
newWorld =
    let
        plane =
            Physics.body
                |> Physics.addShape Physics.plane
                |> Physics.offsetBy (vec3 0 0 -3)
    in
    Physics.world
        |> Physics.setGravity (vec3 0 0 -10)
        |> Physics.addBody
            (Physics.body
                |> Physics.addShape Physics.plane
                |> Physics.offsetBy (vec3 0 0 -3)
            )
        |> Physics.addBody
            (plane
                |> Physics.offsetBy (vec3 -delta 0 0)
                |> Physics.rotateBy Vec3.j angle
            )
        |> Physics.addBody
            (plane
                |> Physics.offsetBy (vec3 delta 0 0)
                |> Physics.rotateBy Vec3.j -angle
            )
        |> Physics.addBody
            (plane
                |> Physics.offsetBy (vec3 0 -delta 0)
                |> Physics.rotateBy Vec3.i -angle
            )
        |> Physics.addBody
            (plane
                |> Physics.offsetBy (vec3 0 delta 0)
                |> Physics.rotateBy Vec3.i angle
            )


init : ( Model, Cmd Msg )
init =
    ( { screenWidth = 1
      , screenHeight = 1
      , world = newWorld
      , devicePixelRatio = 2
      , texture = Nothing
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        , Random.generate NewWorld randomWorld
        , Task.attempt TextureLoaded (Texture.load "dice.png")
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        Resize { width, height } ->
            ( { model
                | screenWidth = toFloat width
                , screenHeight = toFloat height
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
        [ Window.resizes Resize
        , AnimationFrame.diffs Tick
        ]



-- View:


view : Model -> Html Msg
view { screenWidth, screenHeight, devicePixelRatio, world, texture } =
    WebGL.toHtml
        [ width (round (screenWidth * devicePixelRatio))
        , height (round (screenHeight * devicePixelRatio))
        , style "display" "block"
        , style "width" (toString screenWidth ++ "px")
        , style "height" (toString screenHeight ++ "px")
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
                    Physics.foldl (addShape camera perspective text) [] world
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


addShape : Mat4 -> Mat4 -> Texture -> { transform : Mat4, bodyId : Int, shapeId : Int } -> List Entity -> List Entity
addShape camera perspective texture { transform, bodyId } =
    (::)
        (WebGL.entity
            vertex
            fragment
            -- This is hardcoded for now, because the plane body is the first added.
            -- TODO: pull the mesh info from somewhere else, using the bodyId and shapeId
            (if bodyId < 5 then
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
    let
        size =
            10
    in
    WebGL.triangles
        (face
            (vec3 size size 0)
            (vec3 -size size 0)
            (vec3 -size -size 0)
            (vec3 size -size 0)
            6
        )


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
