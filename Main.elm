import Html.App as App
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, x, y)
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
    { padX : Int }


init : (Model, Cmd Msg)
init =
    ({padX = 0}, Cmd.none)



-- UPDATE


type Msg
    = PadMove Position


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        PadMove position ->
            ({ model | padX = position.x }
            , Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    let
        bound = gameAttributes.width - padAttributes.width
        padX = clamp 0 bound model.padX
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
                [ pad padX ]
            ]


(=>) = (,)


px : Int -> String
px pixels =
    toString pixels ++ "px"


gameAttributes =
    { width = 800
    , height = 600
    }


padAttributes =
    { width = 80
    , height = 20
    , top = 500
    }


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves PadMove

