module Model exposing (..)


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

 
updateDestination : EntityPosition -> Coord -> EntityPosition
updateDestination position coord =
  {position| destination = coord}

updatePlayerDestination : Player -> Coord -> Player
updatePlayerDestination player coord =
  let p = player.position in
  {player | position = updateDestination p coord}