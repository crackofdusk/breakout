import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, x, y, cx, cy, r)
import Mouse exposing (Position)
import String


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
    , launched : Bool
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


init : (Model, Cmd Msg)
init =
    let
        midX = round (toFloat gameAttributes.width / 2)
        halfPadWdith = round (toFloat padAttributes.width / 2)
        model =
            { padX = midX - halfPadWdith
            , ballX = midX
            , ballY = padAttributes.top - ballAttributes.radius
            , launched = False
            }
    in
        (model, Cmd.none)


-- UPDATE


type Msg
    = PadMove Position
    | Launch Position


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        PadMove position ->
            let
                halfPadWidth = round (toFloat padAttributes.width / 2)
                x = position.x - halfPadWidth
                bound = gameAttributes.width - padAttributes.width
                boundedX = clamp 0 bound x
                ballX =
                    if model.launched then
                        model.ballX
                    else
                        boundedX + halfPadWidth
            in
                ({ model
                    | padX = boundedX
                    , ballX = ballX
                 }
                , Cmd.none)

        Launch position ->
            ({ model | launched = True }, Cmd.none)


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
    Sub.batch [ Mouse.moves PadMove, Mouse.ups Launch ]

