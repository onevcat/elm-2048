import Browser
import Array exposing (Array)
import Html exposing (..)


--element : { init : flags → (model, unknown), view : model → Html msg, update : msg → model → (model, unknown), subscriptions : model → unknown } → unknown
main = Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type Msg =
    None

type alias Model =
    { board : Board
    }

type alias Board = Array Row
type alias Row = Array Cell
type Cell
    = Tile Int
    | Empty

init : () -> (Model, Cmd Msg)
init _ =
    ( Model <| Array.repeat 4 <| Array.repeat 4 <| Tile 1
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "2048" ]
        , viewBoard model.board
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

viewBoard : Board -> Html Msg
viewBoard board =
    div []
        <| Array.toList <| Array.map viewRow board

viewRow : Row -> Html Msg
viewRow row =
    div []
        <| Array.toList <| Array.map viewCell row

viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
      Tile num -> span [] [ text <| String.fromInt num ]
      Empty -> span [] []