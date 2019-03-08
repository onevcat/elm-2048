import Browser
import Array exposing (Array)
import Html exposing (..)
import Html.Events exposing (..)

--element : { init : flags → (model, unknown), view : model → Html msg, update : msg → model → (model, unknown), subscriptions : model → unknown } → unknown
main = Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type Msg =
    Change

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
update msg model =
    case msg of
        Change ->
            ( { model | board = setBoard (1,2) (Tile 4) model.board }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "2048" ]
        , viewBoard model.board
        , button [ onClick Change] [ text "Click Me" ]
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

type alias Position = (Int, Int)

setBoard : Position -> Cell -> Board -> Board
setBoard (x, y) cell board =
    Array.get x board
      |> Maybe.map (\oldRow -> Array.set y cell oldRow)
      |> Maybe.map (\newRow -> Array.set x newRow board)
      |> Maybe.withDefault board
