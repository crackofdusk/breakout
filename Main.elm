import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, x, y, cx, cy, r)
import Mouse exposing (Position)
import AnimationFrame
import Time exposing (Time)


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
    , ballX : Int
    , ballY : Int
    , ballSpeed : Speed
    , launched : Bool
    }


-- TODO: floats?
-- TODO: use Vec2
type alias Speed =
    { x : Int
    , y : Int
    }


gameAttributes =
    { width = 800
    , height = 600
    }


padAttributes =
    { width = 80
    , height = 20
    , top = 500
    }


ballAttributes =
    { radius = 10 }


launchSpeed : Speed
launchSpeed =
    { x = 1
    , y = -2
    }


init : (Model, Cmd Msg)
init =
    let
        midX = round (toFloat gameAttributes.width / 2)
        halfPadWdith = round (toFloat padAttributes.width / 2)
        model =
            { padX = midX - halfPadWdith
            , ballX = midX
            , ballY = padAttributes.top - ballAttributes.radius
            , ballSpeed = {x = 0, y = 0}
            , launched = False
            }
    in
        (model, Cmd.none)



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
        ballX = model.padX + halfPadWidth
    in
        { model |
            ballX = ballX }


launchBall : Model -> Model
launchBall model =
    if model.launched then
        model
    else
        { model
            | launched = True
            , ballSpeed = launchSpeed
        }


moveBall : Time -> Model -> Model
moveBall delta model =
    if delta > 0 then
        { model
            | ballX = model.ballX + model.ballSpeed.x
            , ballY = model.ballY + model.ballSpeed.y
        }
    else
        model


collideBall : Model -> Model
collideBall =
    collideBallWithWalls >> collideBallWithPad


collideBallWithWalls : Model -> Model
collideBallWithWalls model =
    let
        x = model.ballX
        y = model.ballY
        r = ballAttributes.radius
        dx = model.ballSpeed.x
        dy = model.ballSpeed.y
        dx' = if (x - r) < 0 || (x + r) > gameAttributes.width then -dx else dx
        dy' = if (y - r) < 0 then -dy else dy
        ballSpeed = { x = dx', y = dy' }
    in
        { model | ballSpeed = ballSpeed }


collideBallWithPad : Model -> Model
collideBallWithPad model =
    let
        x = model.ballX
        y = model.ballY
        r = ballAttributes.radius
        dx = model.ballSpeed.x
        dy = model.ballSpeed.y
        dy' =
            if y + r > padAttributes.top && x >= model.padX && x <= model.padX + padAttributes.width then
                -dy
            else
                dy
        ballSpeed = { x = dx, y = dy' }
    in
        { model | ballSpeed = ballSpeed }



-- VIEW


view : Model -> Html Msg
view model =
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
            [ pad model.padX
            , ball model.ballX model.ballY
            ]
        ]


(=>) = (,)


px : Int -> String
px pixels =
    toString pixels ++ "px"


block : Int -> Int -> Int -> Int -> Svg Msg
block x' y' width' height' =
    rect
        [ x (toString x')
        , y (toString y')
        , width (toString width')
        , height (toString height')
        ]
        []


pad : Int -> Svg Msg
pad x =
    block x padAttributes.top padAttributes.width padAttributes.height


ball : Int -> Int -> Svg Msg
ball x y =
    circle
        [ cx (toString x)
        , cy (toString y)
        , r (toString ballAttributes.radius)
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves PadMove
        , Mouse.ups Launch
        , AnimationFrame.diffs TimeLapse
        ]

