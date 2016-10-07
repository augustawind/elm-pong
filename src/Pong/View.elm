module Pong.View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Pong.Model exposing (..)


view : Model -> Html msg
view model =
  let
    boxAttrs =
      [ width (toString model.width)
      , height (toString model.height)
      , x "0", y "0"
      ]

    (leftScoreView, rightScoreView) = viewScores model

  in
    svg boxAttrs
      [ rect (fill model.backgroundColor :: boxAttrs) []
      , viewBall model.ball
      , viewPlayer model.leftPlayer
      , viewPlayer model.rightPlayer
      , leftScoreView
      , rightScoreView
      ]


viewPlayer : Player -> Html msg
viewPlayer player =
  let
    playerWidth = toString player.width
    playerHeight = toString player.height
    playerX = toString player.pos.x
    playerY = toString player.pos.y
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
    ballX = toString ball.pos.x
    ballY = toString ball.pos.y
  in
    circle [ cx ballX, cy ballY, r radius, fill ball.color ] []


viewScores : Model -> (Svg a, Svg a)
viewScores model =
  let
    { leftPlayer, rightPlayer } = model

    midX = model.width // 2
    midY = model.height // 2
    leftX = midX // 2
    rightX = midX + leftX

    textWidth = 50

    leftScore = leftPlayer.score
    leftColor = leftPlayer.color

    rightScore = rightPlayer.score
    rightColor = rightPlayer.color
  in
    ( viewScore leftColor leftScore leftX midY
    , viewScore rightColor rightScore rightX midY
    )


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
