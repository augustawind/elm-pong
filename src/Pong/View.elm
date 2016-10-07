module Pong.View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Pong.Model exposing (..)


view : Model -> Html msg
view model =
  let
    { court, ball, leftPlayer, rightPlayer } = model

    boxAttrs =
      [ width (toString court.width)
      , height (toString court.height)
      , x (toString court.origin.x)
      , y (toString court.origin.y)
      ]

    (leftScoreView, rightScoreView) = viewScores model
  in
    svg boxAttrs
      [ rect (fill model.court.color :: boxAttrs) []
      , viewBall ball
      , viewPlayer leftPlayer
      , viewPlayer rightPlayer
      , leftScoreView
      , rightScoreView
      ]


viewPlayer : Player -> Html msg
viewPlayer player =
  let
    playerWidth = toString player.width
    playerHeight = toString player.height
    playerX = toString player.origin.x
    playerY = toString player.origin.y
  in
    rect
      [ width playerWidth
      , height playerHeight
      , x playerX
      , y playerY
      , fill player.color
      ] []


viewBall : Ball -> Html msg
viewBall ball =
  let
    radius = toString (ball.size // 2)
    ballX = toString ball.origin.x
    ballY = toString ball.origin.y
  in
    circle [ cx ballX, cy ballY, r radius, fill ball.color ] []


viewScores : Model -> (Svg a, Svg a)
viewScores model =
  (text "foo", text "bar")
  {-
  let
    { leftPlayer, rightPlayer } = model

    { court } = model
    { origin } = court

    midX = (origin.x + court.width) // 2
    midY = (origin.y + court.height) // 2
    leftX = (origin.x + midX) // 2
    rightX = origin.x + midX + leftX

    leftScore = leftPlayer.score
    leftColor = leftPlayer.color

    rightScore = rightPlayer.score
    rightColor = rightPlayer.color
  in
    ( viewScore leftColor leftScore leftX midY
    , viewScore rightColor rightScore rightX midY
    )
  -}


viewScore : String -> Int -> Int -> Int -> Svg a
viewScore textColor score x0 y0 =
  let
    textSize = 20
  in
    text'
      [ x (toString x0)
      , y (toString y0)
      , fontFamily "monospace"
      , fontSize (toString textSize)
      , color textColor
      ]
      [ text (toString score) ]
