import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
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

type alias EntityPosition =
    { position : Coord
    , previousPosition : Coord
    , destination : Coord
    }

makePosition coord =
    { position = coord
    , previousPosition = coord
    , destination = coord
    }

type alias Ghost =
    { position : EntityPosition
    }

type alias Player =
    { viewPower : Float
    , position : EntityPosition
    }

type alias Model =
    { ghosts : List Ghost
    , player : Player
    }

init = ({ ghosts = []
        , player =
          { viewPower = 0
          , position =
            { position = { x = 0, y = 0 }
            , previousPosition = { x = 0, y = 0 }
            , destination = { x = 0, y = 0 }
          }}
        }, Random.generate SetGhosts <| makeGhosts 5
        )

distance : Coord -> Coord -> Float
distance start end =
    sqrt <| (start.x - end.x)*(start.x - end.x) + (start.y - end.y)*(start.y - end.y)

generator = Random.pair (Random.float 0 490) (Random.float 0 490)

makeGhost : (Float, Float) -> Ghost
makeGhost (a,b) =
    Ghost <|
      makePosition
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
  | Move Coord

updateDestination : EntityPosition -> Coord -> EntityPosition
updateDestination position coord =
  {position| destination = coord}

updatePlayerDestination : Player -> Coord -> Player
updatePlayerDestination player coord =
  let p = player.position in
  {player | position = updateDestination p coord}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetGhosts ghosts -> ({ model | ghosts = ghosts}, Cmd.none)
    Move dest ->
      let p = model.player in
      ({ model | player = (updatePlayerDestination p dest)}, Cmd.none)

positionToRect position =
    rect [ x (toString position.position.x)
         , y (toString position.position.y)
         , height "10"
         , width "10"
         , fill "red"
         ] []

view : Model -> Html Msg
view model =
      div []
        [ Svg.svg
            [ version "1.1"
            , height "490"
            , width "490"
            ]
            ( rect [x "240", y "240", width "10", height "10", fill "black"] []
             :: List.map (\g -> positionToRect g.position) model.ghosts )
        ]
