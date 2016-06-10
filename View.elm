module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)

import Model exposing (..)


type Msg =
  SetGhosts (List Ghost)
  | Move Coord


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
