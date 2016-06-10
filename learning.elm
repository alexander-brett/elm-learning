import Html.App as Html
import Basics exposing (..)
import List exposing (map)
import Random exposing (..)

import Model exposing (..)
import View exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetGhosts ghosts -> ({ model | ghosts = ghosts}, Cmd.none)
    Move dest ->
      let p = model.player in
      ({ model | player = (updatePlayerDestination p dest)}, Cmd.none)
