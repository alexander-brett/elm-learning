import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onClick)
import Basics exposing (..)
import List exposing (map)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)


main =
  Html.beginnerProgram { model = model, view = view, update = update }

tileSize = 10

type alias Coord =
    { x: Float
    , y: Float
    }

type alias Ghost =
    { position : Coord
    }

type alias Player =
    { viewPower : Float
    , position : Coord
    }

type alias Model = 
    { ghosts : List(Ghost)
    , player : Player
    }

distance : Coord -> Coord -> Float
distance start end =
    sqrt <| (start.x - end.x)*(start.x - end.x) + (start.y - end.y)*(start.y - end.y)

generator = Random.pair (Random.int -240, 240) (Random.int -240, 240)

makeGhost : (Int, Int) -> Ghost
makeGhost (a,b) =
    { position =
        { x = a
        , y = x}
    }

makeGhosts : Int -> List(Ghost)
makeGhosts num = 
    List.map makeGhost (Random.list num generator)

model =
    { ghosts = []
    , player =
        { viewPower = 0
        , position =
            { x = 0
            , y = 0
            }
        }
    }

type alias Msg = {}

update : Msg -> Model -> Model
update msg model = 
    model

makeViewBox =
     "0 0 " ++ (toString <| tileSize*49) ++ " " ++ (toString <| tileSize*49)

ghostToRect ghost =
    rect [x (toString ghost.position.x), y (toString ghost.postition.y), height "10", width "10", fill "red"] []

view : Model -> Html Msg
view model =
    div [] 
        [ Svg.svg
            [ version "1.1"
            , height "490"
            , width "490"
            , Svg.Attributes.style "transform:translate(245px,245px)"    ]
            [ rect [x "0", y "0", width "10", height "10", fill "black"] []
            , List.map ghostToRect <| makeGhosts 5
            ]
        ]
