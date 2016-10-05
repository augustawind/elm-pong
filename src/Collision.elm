module Collision exposing (..)


import List


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


{-
getBoxCollision : Box -> Box -> Maybe Collision
getBoxCollision box1 box2 =
  let
    cx1 = box1.centerX
    cy1 = box1.centerY
    cx2 = box2.centerX
    cy2 = box2.centerY

    rightCollision = box1.right >= box2.left && box1.left <= box2.left 
      && abs (cx1 - cx2) >= abs (cy1 - cy2)
    leftCollision = box1.left <= box2.right && box1.right >= box2.right
      && abs (cx1 - cx2) >= abs (cy1 - cy2)

    topCollision = box1.top <= box2.bottom && box1.bottom >= box2.bottom
      && abs (cy1 - cy2) >= abs (cx1 - cx2)
    bottomCollision = box1.bottom >= box2.top && box1.top <= box2.top
      && abs (cy1 - cy2) >= abs (cx1 - cx2)

    xCollision = rightCollision || leftCollision
    yCollision = topCollision || bottomCollision
  in
    parseCollision xCollision yCollision


getBoundsCollision : Box -> Box -> Maybe Collision
getBoundsCollision innerBox outerBox =
  let
    xCollision = innerBox.left <= outerBox.left || innerBox.right >= outerBox.right
    yCollision = innerBox.top <= outerBox.top || innerBox.bottom >= outerBox.bottom
  in
    parseCollision xCollision yCollision


parseCollision : Bool -> Bool -> Maybe Collision
parseCollision xCollision yCollision =
  if xCollision && yCollision then
    Just XYCollision
  else if xCollision then
    Just XCollision
  else if yCollision then
    Just YCollision
  else
    Nothing


getCollision : Box -> Box -> List (Box, Box) -> Maybe Collision
getCollision innerBox outerBox boxes =
  let
    boundsCollision = getBoundsCollision innerBox outerBox
    boxCollisions = List.map (uncurry getBoxCollision) boxes
  in
    List.foldr mergeCollisions Nothing (boundsCollision :: boxCollisions)


mergeCollisions : Maybe Collision -> Maybe Collision -> Maybe Collision
mergeCollisions coll1 coll2 =
  case (coll1, coll2) of
    (Just XYCollision, _) -> Just XYCollision
    (_, Just XYCollision) -> Just XYCollision
    (Just XCollision, Just YCollision) -> Just XYCollision
    (Just YCollision, Just XCollision) -> Just XYCollision
    (something, Nothing) -> something
    (Nothing, something) -> something
    (something, _) -> something
-}
