module Pong.Collision exposing (Collision(..), getCollision)

import Pong.Model exposing (..)


type Collision
  = LeftPaddleCollision
  | RightPaddleCollision
  | LeftCollision
  | RightCollision
  | TopCollision
  | BottomCollision


type alias Box =
  { top : Int
  , right : Int
  , bottom : Int
  , left : Int
  , centerX : Int
  , centerY : Int
  }


type alias BallBox = Box
type alias LeftPaddleBox = Box
type alias RightPaddleBox = Box
type alias BoundsBox = Box


getCollision : Model -> Maybe Collision
getCollision model =
  let
    { leftPlayer, rightPlayer } = model

    leftPlayerBox =
      let
        { pos, width, height } = leftPlayer
        { x, y } = pos
      in
        getRectBox x y width height

    rightPlayerBox =
      let
        { pos, width, height } = rightPlayer
        { x, y } = pos
      in
        getRectBox x y width height

    ballBox =
      let
        { pos, size } = model.ball
        { x, y } = pos
        radius = size // 2
      in
        getCircleBox x y radius

    boundsBox = getRectBox 0 0 model.width model.height

  in
    getBoxCollision ballBox leftPlayerBox rightPlayerBox boundsBox


getBoxCollision : BallBox -> LeftPaddleBox -> RightPaddleBox -> BoundsBox -> Maybe Collision
getBoxCollision ball leftPaddle rightPaddle bounds =
  if getLeftPaddleCollision ball leftPaddle then
    Just LeftPaddleCollision
  else if getRightPaddleCollision ball rightPaddle then
    Just RightPaddleCollision
  else if ball.top <= bounds.top then
    Just TopCollision
  else if ball.bottom >= bounds.bottom then
    Just BottomCollision
  else if ball.left <= bounds.left then
    Just LeftCollision
  else if ball.right >= bounds.right then
    Just RightCollision
  else
    Nothing


getLeftPaddleCollision : BallBox -> LeftPaddleBox -> Bool
getLeftPaddleCollision ball paddle =
  (ball.top <= paddle.bottom && ball.bottom >= paddle.top) &&
  (ball.left <= paddle.right && ball.right >= paddle.right)


getRightPaddleCollision : BallBox -> RightPaddleBox -> Bool
getRightPaddleCollision ball paddle =
  (ball.top <= paddle.bottom && ball.bottom >= paddle.top) &&
  (ball.right >= paddle.left && ball.left <= paddle.left)


getRectBox : Int -> Int -> Int -> Int -> Box
getRectBox x0 y0 width height =
  { top = y0, bottom = y0 + height
  , left = x0, right = x0 + width
  , centerX = (width - x0) // 2
  , centerY = (height - y0) // 2
  }


getCircleBox : Int -> Int -> Int -> Box
getCircleBox cx cy radius =
  { top = cy - radius, bottom = cy + radius
  , left = cx - radius, right = cx + radius
  , centerX = cx, centerY = cy
  }
