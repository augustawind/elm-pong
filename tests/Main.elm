port module Main exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)

import Tests exposing (..)


main : Program Value
main = 
  run emit Tests.collisionTests


port emit : ( String, Value ) -> Cmd msg
