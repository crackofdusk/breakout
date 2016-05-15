import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, x, y, rx, cx, cy, r, stroke, strokeWidth, fill)
import Mouse
import AnimationFrame
import Time exposing (Time)
import Vec2 exposing (Vec2)


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
    collideBallWithWalls >> collideBallWithPad


collideBallWithWalls : Model -> Model
collideBallWithWalls model =
    let
        {x, y} = model.ballPosition
        r = ballAttributes.radius
        dx = model.ballVelocity.x
        dy = model.ballVelocity.y
        dx' = if (x - r) < 0 || (x + r) > gameAttributes.width then -dx else dx
        dy' = if (y - r) < 0 then -dy else dy
        ballVelocity = { x = dx', y = dy' }
    in
        { model | ballVelocity = ballVelocity }


collideBallWithPad : Model -> Model
collideBallWithPad model =
    let
        {x, y} = model.ballPosition
        r = ballAttributes.radius
        dx = model.ballVelocity.x
        dy = model.ballVelocity.y
        dy' =
            if y + r > padAttributes.top && x >= model.padX && x <= model.padX + padAttributes.width then
                -dy
            else
                dy
        ballVelocity = { x = dx, y = dy' }
    in
        { model | ballVelocity = ballVelocity }



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

