module Collision exposing (..)


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


getCollision : BallBox -> LeftPaddleBox -> RightPaddleBox -> BoundsBox -> Maybe Collision
getCollision ball leftPaddle rightPaddle bounds =
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
  (ball.top <= paddle.bottom || ball.bottom >= paddle.top) &&
  (ball.left <= paddle.right && ball.right >= paddle.right)


getRightPaddleCollision : BallBox -> RightPaddleBox -> Bool
getRightPaddleCollision ball paddle =
  (ball.top <= paddle.bottom || ball.bottom >= paddle.top) &&
  (ball.right >= paddle.left && ball.left <= paddle.left)
