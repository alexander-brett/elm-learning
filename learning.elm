import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onInput, onClick)
import Basics exposing (..)
import List exposing (map)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


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
    { ghosts : List Ghost
    , player : Player
    }

init = ({ ghosts = []
        , player =
          { viewPower = 0
          , position =
            { x = 0
            , y = 0
            }
          }
        }, Random.generate SetGhosts <| makeGhosts 5
        )

distance : Coord -> Coord -> Float
distance start end =
    sqrt <| (start.x - end.x)*(start.x - end.x) + (start.y - end.y)*(start.y - end.y)

generator = Random.pair (Random.float -240 240) (Random.float -240 240)

makeGhost : (Float, Float) -> Ghost
makeGhost (a,b) =
    Ghost
        { x = a
        , y = b}
    

makeGhosts : Int -> Generator (List Ghost)
makeGhosts num = 
    Random.map (\l -> List.map makeGhost l) (Random.list num generator)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg = 
  SetGhosts (List Ghost)
  
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetGhosts ghosts -> ({ model | ghosts = ghosts}, Cmd.none)

ghostToRect ghost =
    rect [x (toString ghost.position.x), y (toString ghost.position.y), height "10", width "10", fill "red"] []

view : Model -> Html Msg
view model =
      div [] 
        [ Svg.svg
            [ version "1.1"
            , height "490"
            , width "490"
            , Svg.Attributes.style "transform:translate(245px,245px)"    ]
            [ rect [x "0", y "0", width "10", height "10", fill "black"] []
            ] ++ List.map ghostToRect model.ghosts
            
        ] 