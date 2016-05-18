import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, x, y, rx, cx, cy, r, stroke, strokeWidth, fill)
import Mouse
import AnimationFrame
import Time exposing (Time)
import Vec2 exposing (Vec2)
import Collision exposing (BoundingBox, Direction(..))


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { padX : Int
    , ballPosition : Position
    , ballVelocity : Velocity
    , launched : Bool
    , bricks: List Brick
    }


type alias Position =
    Vec2 Int


type alias Velocity =
    Vec2 Int


type alias Brick =
    { position : Position
    , width : Int
    , height : Int
    }


gameAttributes =
    { width = 800
    , height = 600
    }


padAttributes =
    { width = 80
    , height = 15
    , top = 500
    }


ballAttributes =
    { radius = 10 }


brickAttributes =
    { width = 60
    , height = 25
    }


launchVelocity : Velocity
launchVelocity =
    { x = 1
    , y = -2
    }


init : (Model, Cmd Msg)
init =
    let
        midX = round (toFloat gameAttributes.width / 2)
        halfPadWdith = round (toFloat padAttributes.width / 2)
        ballPosition =
            { x = midX
            , y = padAttributes.top - ballAttributes.radius
            }
        model =
            { padX = midX - halfPadWdith
            , ballPosition = ballPosition
            , ballVelocity = {x = 0, y = 0}
            , launched = False
            , bricks = initBricks
            }
    in
        (model, Cmd.none)


initBricks : List Brick
initBricks =
    let
        initial =
            List.repeat 50
                { position = { x = 0, y = 0 }
                , width = brickAttributes.width
                , height = brickAttributes.height
                }
        positioned = List.indexedMap assignBrickPosition initial
    in
        positioned


assignBrickPosition index b =
    let
        xOffset = 100
        yOffset = 80
        padding = 10
        lineLength = 10
        x = (rem index lineLength) * (b.width) + xOffset
        y = (index // lineLength) * (b.height + padding) + yOffset
    in
        { b | position = { x = x , y = y }}

-- UPDATE


type Msg
    = PadMove Position
    | Launch Position
    | TimeLapse Time


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        PadMove position ->
            (model
                |> updatePad position.x
                |> updateBall 0
            , Cmd.none)

        Launch _ ->
            (launchBall model, Cmd.none)

        TimeLapse delta ->
            (model
                |> updateBall delta
            , Cmd.none)



updatePad : Int -> Model -> Model
updatePad mouseX model =
    let
        halfPadWidth = round (toFloat padAttributes.width / 2)
        x = mouseX - halfPadWidth
        bound = gameAttributes.width - padAttributes.width
        boundedX = clamp 0 bound x
    in
        { model | padX = boundedX }


updateBall : Time -> Model -> Model
updateBall delta model =
    if model.launched then
        model
            |> moveBall delta
            |> collideBall
    else
        followPadWithBall model


followPadWithBall : Model -> Model
followPadWithBall model =
    let
        halfPadWidth = round (toFloat padAttributes.width / 2)
        ballPosition =
            { x = model.padX + halfPadWidth
            , y = model.ballPosition.y
            }
    in
        { model |
            ballPosition = ballPosition }


launchBall : Model -> Model
launchBall model =
    if model.launched then
        model
    else
        { model
            | launched = True
            , ballVelocity = launchVelocity
        }


moveBall : Time -> Model -> Model
moveBall delta model =
    if delta > 0 then
        { model
            | ballPosition = Vec2.add model.ballPosition model.ballVelocity
        }
    else
        model


collideBall : Model -> Model
collideBall =
    collideBallWithWalls >> collideBallWithPad >> collideBallWithBricks


collideBallWithWalls : Model -> Model
collideBallWithWalls model =
    let
        ball = ballToCircle model
        walls = wallBoundingBoxes
        collide = \wall model' -> collideBallWithBox ball wall model'
    in
        List.foldl collide model walls


collideBallWithPad : Model -> Model
collideBallWithPad model =
    let
        ball = ballToCircle model
        box = padToBoundingBox model
    in
        collideBallWithBox ball box model


collideBallWithBricks : Model -> Model
collideBallWithBricks model =
    let
        ball = ballToCircle model
        collision = \brick -> Collision.circleVsAABB ball (brickToBoundingBox brick)
        isColliding = \brick -> .isColliding (collision brick)
        (candidates, notColliding) = List.partition isColliding model.bricks
    in
        case candidates of
            brick :: rest ->
                let
                    model' = collideBallWithBox ball (brickToBoundingBox brick) model
                in
                    -- destroying more than one brick produces a buggy rebound
                    { model' | bricks = List.append rest notColliding }

            [] ->
                model


collideBallWithBox : Collision.Circle -> BoundingBox -> Model -> Model
collideBallWithBox ball box model =
    let
        collision = Collision.circleVsAABB ball box
    in
        if collision.isColliding then
            let
                rebound = Collision.reboundDirection collision.penetration
                correctedVelocity = reboundVelocity rebound model
                correctedPosition = Vec2.minus model.ballPosition collision.penetration
            in
                { model
                    | ballPosition = correctedPosition
                    , ballVelocity = correctedVelocity
                }
        else
            model


reboundVelocity : Direction -> Model -> Velocity
reboundVelocity direction model =
    let
        dx = model.ballVelocity.x
        dy = model.ballVelocity.y
    in
        case direction of
            Up ->
                { x = dx, y = -dy }

            Down ->
                { x = dx, y = -dy }

            Left ->
                { x = -dx, y = dy }

            Right ->
                { x = -dx, y = dy }


padToBoundingBox : Model -> BoundingBox
padToBoundingBox model =
    { topLeft =
        { x = model.padX
        , y = padAttributes.top
        }
    , bottomRight =
        { x = model.padX + padAttributes.width
        , y = padAttributes.top + padAttributes.height
        }
    }


brickToBoundingBox : Brick -> BoundingBox
brickToBoundingBox brick =
    let
        topLeft = brick.position
        bottomRight =
            Vec2.add brick.position { x = brick.width, y = brick.height }
    in
        { topLeft = topLeft
        , bottomRight = bottomRight
        }


ballToCircle : Model -> Collision.Circle
ballToCircle model =
    { center = model.ballPosition
    , radius = ballAttributes.radius
    }


wallBoundingBoxes : List (BoundingBox)
wallBoundingBoxes =
    let
        wallWidth = 50
        worldWidth = gameAttributes.width
        worldHeight = gameAttributes.height
    in
        -- left wall
        [ { topLeft = { x = -wallWidth, y = -wallWidth }
          , bottomRight = { x = 0, y = worldHeight }
          }
          -- top wall
        , { topLeft = { x = 0, y = -wallWidth }
          , bottomRight = { x = worldWidth, y = 0 }
          }
          -- right wall
        , { topLeft = { x = worldWidth , y = -wallWidth }
          , bottomRight = { x = worldWidth + wallWidth, y = worldHeight }
          }
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        nodes =
            pad model.padX ::
            ball model.ballPosition.x model.ballPosition.y ::
            List.map brick model.bricks
    in
        div
            [ style
                [ "width" => px gameAttributes.width
                , "height" => px gameAttributes.height
                , "background-color" => "#efe"
                ]
            ]
            [ svg
                [ width (toString gameAttributes.width)
                , height (toString gameAttributes.height)
                ]
                nodes
            ]


(=>) = (,)


px : Int -> String
px pixels =
    toString pixels ++ "px"


block : Int -> Int -> Int -> Int -> List (Svg.Attribute Msg) -> Svg Msg
block x' y' width' height' extraAttributes =
    rect
        (List.append
            [ x (toString x')
            , y (toString y')
            , width (toString width')
            , height (toString height')
            ]
            extraAttributes)
        []


pad : Int -> Svg Msg
pad x =
    block x padAttributes.top padAttributes.width padAttributes.height
        [ fill "#632F53"
        , rx "3"
        ]


ball : Int -> Int -> Svg Msg
ball x y =
    circle
        [ cx (toString x)
        , cy (toString y)
        , r (toString ballAttributes.radius)
        , fill "#632F53"
        ]
        []


brick : Brick -> Svg Msg
brick b =
    let
        styles =
            [ stroke "#5EBCCF"
            , strokeWidth "3"
            , fill "#69D2E7"
            , rx "2"
            ]
    in
        block b.position.x b.position.y b.width b.height styles



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves PadMove
        , Mouse.ups Launch
        , AnimationFrame.diffs TimeLapse
        ]

