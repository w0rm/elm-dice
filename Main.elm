module Dice exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL.Texture as Texture exposing (Error, Texture)
import Physics
import Task
import Time exposing (Time)
import WebGL exposing (Entity, Shader, Mesh)
import Window
import Random


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
    5


angle : Float
angle =
    pi / 4


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
      , devicePixelRatio = 1
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
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0 0 0 1
        ]
        [ width (round (screenWidth * devicePixelRatio))
        , height (round (screenHeight * devicePixelRatio))
        , style
            [ ( "display", "block" )
            , ( "width", toString screenWidth ++ "px" )
            , ( "height", toString screenHeight ++ "px" )
            ]
        , onClick Restart
        ]
        (let
            camera =
                Mat4.makeLookAt (Vec3.vec3 0 0.1 25) (Vec3.vec3 0 0 0) Vec3.k

            perspective =
                Mat4.makePerspective 24 (screenWidth / screenHeight) 5 2000

            entities =
                texture
                    |> Maybe.map
                        (\text ->
                            Physics.foldl (addShape camera perspective text) [] world
                        )
                    |> Maybe.withDefault []

            -- Uncomment to see collision points
            -- |> addContacts camera perspective (Physics.contacts world)
         in
            entities
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
    , transformNormals : Mat4
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
            (-- This is hardcoded for now, because the plane body is the first added.
             -- TODO: pull the mesh info from somewhere else, using the bodyId and shapeId
             if bodyId < 5 then
                planeMesh
             else
                cubeMesh
            )
            { transform = transform
            , transformNormals =
                Mat4.transpose (Mat4.inverse transform |> Maybe.withDefault Mat4.identity)
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
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        [ face rft rfb rbb rbt 0
        , face rft rfb lfb lft 1
        , face rft lft lbt rbt 2
        , face rfb lfb lbb rbb 3
        , face lft lfb lbb lbt 4
        , face rbt rbb lbb lbt 5
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
        attribute highp float textureNumber;
        attribute vec3 normal;
        uniform mat4 camera;
        uniform mat4 perspective;
        uniform mat4 transform;
        uniform mat4 transformNormals;
        uniform sampler2D texture;
        varying vec3 vTexturePosition;
        varying float vTextureNumber;
        varying float vlighting;
        highp float ambientLight = 0.4;
        highp float directionalLight = 0.6;
        highp vec3 directionalVector = vec3(0, 0, 1);

        void main () {
          gl_Position = perspective * camera * transform * vec4(position, 1.0);
          highp vec3 transformedNormal = normalize(mat3(transformNormals) * normal);
          highp float directional = max(dot(transformedNormal, directionalVector), 0.0);
          vlighting = 1.0; //ambientLight + directional * directionalLight;
          vTextureNumber = textureNumber;
          vTexturePosition = texturePosition;
        }
    |]


fragment : Shader {} Uniforms Varying
fragment =
    [glsl|
        precision mediump float;
        varying vec3 vTexturePosition;
        varying float vTextureNumber;
        varying float vlighting;
        uniform sampler2D texture;
        void main () {
          if (vTextureNumber < 6.0) {
            gl_FragColor = texture2D(texture, vec2((vTexturePosition.x + vTextureNumber) / 8.0, vTexturePosition.y));
          } else {
            gl_FragColor = vec4(vlighting * vec3(0.4, 0.4, 0.4), 1.0);
          }
        }
    |]
