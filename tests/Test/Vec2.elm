module Test.Vec2 exposing (tests)

import ElmTest exposing (..)
import Check exposing (Claim, Evidence, claim, true, that, is, for, quickCheck)
import Check.Producer exposing (Producer, convert, tuple, tuple3, float)
import Check.Test
import Vec2 exposing (Vec2)


tests : Test
tests =
    suite "Vec2"
        [ propertyTests
        , exampleTests
        ]


-- Property tests

propertyTests : Test
propertyTests =
    Check.Test.evidenceToTest (quickCheck claims)


claims : Claim
claims =
    Check.suite "Properties"
        [ addition_commutativity
        , addition_associativity
        , addition_identity
        ]


addition_commutativity : Claim
addition_commutativity =
    claim "addition is commutative"
        `that`
            (\(x,y) -> Vec2.add x y)
        `is`
            (\(x,y) -> Vec2.add y x)
        `for`
            tuple(vector2, vector2)


addition_associativity : Claim
addition_associativity =
    claim "addition is associative"
        `true`
            (\(x,y,z) ->
                assertEqualInDelta
                    (Vec2.add x (Vec2.add y z))
                    (Vec2.add (Vec2.add x y) z))
        `for`
            tuple3(vector2, vector2, vector2)


addition_identity : Claim
addition_identity =
    claim "the zero vector2 is the identity element of vector2 addition"
        `that`
            (\x -> Vec2.add x { x = 0, y = 0 })
        `is`
            identity
        `for`
            vector2


vector2 : Producer Vec2
vector2 =
    Check.Producer.map (\(x,y) -> { x = x, y = y }) (tuple (float, float))


assertEqualInDelta : Vec2 -> Vec2 -> Bool
assertEqualInDelta v1 v2 =
    let delta = 1E-8
    in
        abs (v1.x - v2.x) < delta
        && abs (v1.y - v2.y) < delta



-- Example tests


exampleTests : Test
exampleTests =
    suite "Examples"
        [ test "addition" <| assertEqual { x = 6, y = 6 } (Vec2.add { x = 2, y = -2 } { x = 4, y = 8 })
        ]
