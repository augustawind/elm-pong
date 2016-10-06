module Pong exposing (..)

import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

import Collision exposing (..)


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


-- MODEL


type alias Model =
  { ball : Ball
  , leftPlayer : Player
  , rightPlayer : Player
  , width : Int
  , height : Int
  , backgroundColor : String
  , framesPerSecond : Float
  }


type alias Ball =
  { pos : Point
  , size : Int
  , velocity : Point
  , color : String
  }


type alias Player =
  { pos : Point
  , width : Int
  , height : Int
  , speed : Int
  , score : Int
  , color : String
  }


type alias Point =
  { x : Int
  , y : Int
  }


-- UPDATE


type Msg
  = MoveBall Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    MoveBall time ->
      let
        collision = calculateCollision model
        newModel = updateScore model collision
        newModel' =
          { newModel | ball = updateBall newModel collision }
      in 
        (newModel', Cmd.none)


calculateCollision : Model -> Maybe Collision
calculateCollision model =
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
    getCollision ballBox leftPlayerBox rightPlayerBox boundsBox


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

    leftPlayer =
      model.leftPlayer

    rightPlayer =
      model.rightPlayer

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
          (1, 1)

        Just RightCollision ->
          -- TODO: handle scoring
          (1, 1)

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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (second / model.framesPerSecond) MoveBall


-- VIEW


view : Model -> Html Msg
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


viewPlayer : Player -> Html Msg
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


viewBall : Ball -> Html Msg
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
    {leftPlayer, rightPlayer} = model

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
