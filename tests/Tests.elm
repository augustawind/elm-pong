module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import Collision exposing (..)


collisionTests : Test
collisionTests =
  describe "Collision"
    [ testBox
    , testParseCollision
    , testGetBoxCollision
    ]
    {-
    , testGetBoundsCollision
    , testMergeCollisions
    , testGetCollision
    ]
    -}


testBox : Test
testBox =
  describe "converting shapes to Boxes"
    [ test "creating a Box from a rectangle" <|
        \() -> testGetRectBox
    , test "creating a Box from a circle" <|
        \() -> testGetCircleBox
    ]


testGetRectBox : Expectation
testGetRectBox =
    let actual = getRectBox 5 10 30 32
        expected = { top = 10, right = 35, bottom = 42, left = 5, centerX = 15, centerY = 16 }
     in Expect.equal actual expected


testGetCircleBox : Expectation
testGetCircleBox =
  let actual = getCircleBox 10 8 6
      expected = { top = 2, right = 16, bottom = 14, left = 4, centerX = 6, centerY = 6 }
  in Expect.equal actual expected


testParseCollision : Test
testParseCollision =
  describe "parsing a Collision value from an X and Y Bool"
    [ test "with both axes colliding" <|
        \() -> Expect.equal (parseCollision True True) (Just XYCollision)
    , test "with only the x axis colliding" <|
        \() -> Expect.equal (parseCollision True False) (Just XCollision)
    , test "with only the y axis colliding" <|
        \() -> Expect.equal (parseCollision False True) (Just YCollision)
    , test "with neither axis colliding" <|
        \() -> Expect.equal (parseCollision False False) (Nothing)
    ]


testGetBoxCollision : Test
testGetBoxCollision =
  describe "collision between two Boxes"
    [ describe "XCollision"
        [ test "vertically aligned" <|
            \() -> let box1 = getRectBox 5 0 10 10
                       box2 = getRectBox 8 0 20 10
                       actual = getBoxCollision box1 box2
                       expected = Just XCollision
                    in Expect.equal actual expected

        , test "not vertically aligned" <|
            \() -> let box1 = getRectBox 5 0 15 10
                       box2 = getRectBox 14 2 15 9
                       actual = getBoxCollision box1 box2
                       expected = Just XCollision
                    in Expect.equal actual expected
        ]
    ]
