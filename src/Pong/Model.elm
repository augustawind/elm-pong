module Pong.Model exposing (..)


type alias Model =
  { court : Court
  , ball : Ball
  , leftPlayer : Player
  , rightPlayer : Player
  , currentRound : Int
  , totalRounds : Int
  , framesPerSecond : Float
  }


type alias Court =
  { origin : Point
  , width : Int
  , height : Int
  , color : String
  }


type alias Ball =
  { origin : Point
  , size : Int
  , velocity : Point
  , color : String
  }


type alias Player =
  { origin : Point
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
