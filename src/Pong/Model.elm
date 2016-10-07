module Pong.Model exposing (..)


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
