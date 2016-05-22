module Main exposing (..)


import ElmTest exposing (..)
import Test.Vec2 as Vec2


tests : Test
tests =
    suite "Project tests"
        [ Vec2.tests
        ]


main =
    runSuite tests
