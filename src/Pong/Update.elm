module Pong.Update exposing (Msg(..), update, nextRound)

import Time exposing (Time)

import Pong.Model exposing (..)
import Pong.Collision exposing (..)


type Msg
  = MoveBall Time
  | NextRound


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
        MoveBall time ->
          let
            collision = getCollision model
            nextModel = updateScore model collision
          in
            { nextModel | ball = updateBall nextModel collision }

        NextRound ->
          nextRound model
  in
    (newModel, Cmd.none)


updateScore : Model -> Maybe Collision -> Model
updateScore model collision =
  let
    { leftPlayer, rightPlayer } = model

    leftScore =
      leftPlayer.score +
        if collision == Just RightCollision then
          1
        else
          0

    rightScore =
      rightPlayer.score +
        if collision == Just LeftCollision then
          1
        else
          0

    newLeftPlayer =
      { leftPlayer | score = leftScore }

    newRightPlayer =
      { rightPlayer | score = rightScore }

    model =
      { model | leftPlayer = newLeftPlayer, rightPlayer = newRightPlayer }
  in
    if leftScore /= leftPlayer.score || rightScore /= rightPlayer.score then
      nextRound model
    else
      model


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

    { ball } = model
    { x, y } = ball.velocity

    newBall =
      { ball | velocity = Point (x * dx) (y * dy) }
   in
     moveBall newBall


moveBall : Ball -> Ball
moveBall ball =
  let
    { x, y } = ball.origin
    vx = ball.velocity.x
    vy = ball.velocity.y
    newPos = Point (x + vx) (y + vy)
  in
    { ball | origin = newPos }


nextRound : Model -> Model
nextRound model =
  let
    { court, ball, leftPlayer, rightPlayer, currentRound } = model
    centerX = (court.origin.x + court.width) // 2
    centerY = (court.origin.y + court.height) // 2

    newBall = { ball | origin = Point centerX centerY }

    newLeftPlayer =
        { leftPlayer | origin = Point leftPlayer.origin.x centerY }

    newRightPlayer =
        { rightPlayer | origin = Point rightPlayer.origin.x centerY }

  in
    { model
      | ball = newBall
      , leftPlayer = newLeftPlayer
      , rightPlayer = newRightPlayer
      , currentRound = currentRound + 1
    }
