import Array exposing (Array)
import Html exposing (..)

main =
    text "text"

type alias Model =
    { board : Board
    }

type alias Board = Array Row
type alias Row = Array Cell
type Cell
    = Tile Int
    | Empty



