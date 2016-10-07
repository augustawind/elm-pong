module Main exposing (..)

import Html.App as App
import Time exposing (Time, second)

import Pong.Collision exposing (..)
import Pong.Model exposing (..)
import Pong.Update exposing (Msg(..), update)
import Pong.View exposing (view)


main : Program Never
main = App.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- INIT


init : (Model, Cmd Msg)
init =
  let
    model =
      { ball = ball
      , leftPlayer = leftPlayer
      , rightPlayer = rightPlayer
      , width = 500
      , height = 500
      , backgroundColor = "#333333"
      , framesPerSecond = 60
      }
  in
    (model, Cmd.none)


ball : Ball
ball =
    { pos = Point 65 65
    , size = 10
    , velocity = Point -2 -1
    , color = "#00FF00"
    }


leftPlayer : Player
leftPlayer =
    { pos = Point 0 50
    , width = 10
    , height = 40
    , speed = 5
    , score = 0
    , color = "#FF0000"
    }


rightPlayer : Player
rightPlayer =
    { pos = Point 300 50
    , width = 10
    , height = 40
    , speed = 5
    , score = 0
    , color = "#0000FF"
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (second / model.framesPerSecond) MoveBall
