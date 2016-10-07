module Pong.Update exposing (Msg(..), update)

import Time exposing (Time)

import Pong.Model exposing (..)
import Pong.Collision exposing (..)


type Msg
  = MoveBall Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MoveBall time ->
      let
        collision = getCollision model
        newModel = updateScore model collision
        newModel' =
          { newModel | ball = updateBall newModel collision }
      in
        (newModel', Cmd.none)


updateScore : Model -> Maybe Collision -> Model
updateScore model collision =
  let
    leftScore =
      model.leftPlayer.score +
        if collision == Just RightCollision then
          1
        else
          0

    rightScore =
      model.rightPlayer.score +
        if collision == Just LeftCollision then
          1
        else
          0

    leftPlayer = model.leftPlayer
    rightPlayer = model.rightPlayer

    newLeftPlayer =
      { leftPlayer | score = leftScore }

    newRightPlayer =
      { rightPlayer | score = rightScore }
  in
    { model | leftPlayer = newLeftPlayer, rightPlayer = newRightPlayer }



updateBall : Model -> Maybe Collision -> Ball
updateBall model collision =
  let
    (dx, dy) =
      case collision of
        Just LeftPaddleCollision ->
          (-1, 1)

        Just RightPaddleCollision ->
          (-1, 1)

        Just TopCollision ->
          (1, -1)

        Just BottomCollision ->
          (1, -1)

        Just LeftCollision ->
          -- TODO: handle scoring
          (-1, 1)

        Just RightCollision ->
          -- TODO: handle scoring
          (-1, 1)

        Nothing ->
          (1, 1)

    newBall =
      let
        ball = model.ball
        { x, y } = ball.velocity
      in
        { ball | velocity = Point (x * dx) (y * dy) }
   in
     moveBall newBall


moveBall : Ball -> Ball
moveBall ball =
  let
    { x, y } = ball.pos
    vx = ball.velocity.x
    vy = ball.velocity.y
    newPos = Point (x + vx) (y + vy)
  in
    { ball | pos = newPos }
