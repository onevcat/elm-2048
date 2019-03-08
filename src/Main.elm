import Browser
import Array exposing (Array)
import Html exposing (..)
import Html.Events exposing (..)
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Random
import Html.Attributes exposing (..)

main = Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

type Direction
    = Left
    | Up
    | Right
    | Down
    | Other

keyDecoder : Decode.Decoder Direction
keyDecoder = Decode.map toDirection <| Decode.field "key" Decode.string

toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft"  -> Left
        "ArrowUp"    -> Up
        "ArrowRight" -> Right
        "ArrowDown"  -> Down
        _            -> Other

type Msg
    = Slide Direction
    | New Position

type alias Model =
    { board : Board
    }

type alias Board = Array Row
type alias Row = Array Cell
type Cell
    = Tile Int
    | Empty

rowCount : Int
rowCount = 4

columnCount : Int
columnCount = 4

init : () -> (Model, Cmd Msg)
init _ =
    ( Model <| Array.repeat rowCount <| Array.repeat columnCount <| Tile 1
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Slide direction ->
            let
              newBoard = slideBoard direction model.board
              cmd = if model.board == newBoard then
                    Cmd.none
                    else
                    randomPositionInEmpty newBoard
                      |> Maybe.map (Random.generate New)
                      |> Maybe.withDefault Cmd.none
            in
              ( { model | board = slideBoard direction model.board }
              , cmd
              )
        New position ->
            ( { model | board = setBoard position (Tile 2) model.board }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "2048" ]
        , viewBoard model.board
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Slide keyDecoder)
        ]

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
      Tile num -> span [ style "padding" "1em" ] [ text <| String.fromInt num ]
      Empty -> span [ style "padding" "1em" ] [ text "_" ]

type alias Position = (Int, Int)

setBoard : Position -> Cell -> Board -> Board
setBoard (x, y) cell board =
    Array.get x board
      |> Maybe.map (\oldRow -> Array.set y cell oldRow)
      |> Maybe.map (\newRow -> Array.set x newRow board)
      |> Maybe.withDefault board

type State
    = Waiting Cell (List Cell)
    | Done (List Cell)

applyRule : Cell -> State -> State
applyRule cell state =
    case cell of
        Empty -> state
        Tile number ->
            case state of
                Waiting waitingCell cells ->
                    if waitingCell == cell then
                      Done (Tile (number + number) :: cells )
                    else
                      Waiting cell (waitingCell :: cells)
                Done cells ->
                    Waiting cell cells

slideBoard : Direction -> Board -> Board
slideBoard direction board =
    board |> listBoard |> slideListBoard direction |> arrayBoard

slideListBoard : Direction -> List (List Cell) -> List (List Cell)
slideListBoard direction board =
    case direction of
        Left ->
            board
              |> List.map List.reverse
              |> slideListBoard Right
              |> List.map List.reverse
        Up ->
            board
              |> transpose
              |> slideListBoard Left
              |> transpose
        Right ->
            board
              |> List.map slideRow
        Down ->
            board
              |> transpose
              |> slideListBoard Right
              |> transpose
        Other ->
            board


slideRow : List Cell -> List Cell
slideRow row =
    let
        initState = List.foldr applyRule (Done []) row
        newRow = case initState of
            Waiting waiting done -> waiting :: done
            Done done -> done
    in
        -- Use Empty to fill up all non tile.
        List.repeat (List.length row - List.length newRow) Empty ++ newRow

listBoard : Board -> List (List Cell)
listBoard board = board |> Array.map Array.toList |> Array.toList

arrayBoard : List (List Cell) -> Board
arrayBoard board = board |> List.map Array.fromList |> Array.fromList

transpose : List (List Cell) -> List (List Cell)
transpose input = List.foldr (List.map2 (::)) (List.repeat (List.length input) []) input

randomPositionInEmpty : Board -> Maybe (Random.Generator Position)
randomPositionInEmpty board =
    case allEmptyPositions board of
        [] -> Nothing
        head :: tail -> Just <| Random.uniform head tail

allEmptyPositions : Board -> List Position
allEmptyPositions board =
    board |> listBoard
          |> List.concat
          |> List.indexedMap Tuple.pair
          |> List.filterMap
              (\(index, cell) ->
                case cell of
                    Tile _ -> Nothing
                    Empty -> Just <| indexToPosition index
              )

indexToPosition : Int -> Position
indexToPosition index = ( index // columnCount, remainderBy columnCount index )