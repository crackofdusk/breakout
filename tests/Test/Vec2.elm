module Test.Vec2 exposing (tests)

import ElmTest exposing (..)
import Check exposing (Claim, Evidence, claim, that, is, for, quickCheck)
import Check.Producer exposing (Producer, convert, tuple, tuple3, float, int)
import Check.Test
import Vec2 exposing (Vec2)


tests : Test
tests =
    suite "Vector"
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
    claim "vector2 addition is commutative"
        `that`
            (\(x,y) -> Vec2.add x y)
        `is`
            (\(x,y) -> Vec2.add y x)
        `for`
            tuple(floatVector2, floatVector2)


addition_associativity : Claim
addition_associativity =
    claim "vector2 addition is associative"
        `that`
            (\(x,y,z) -> Vec2.add x (Vec2.add y z))
        `is`
            (\(x,y,z) -> Vec2.add (Vec2.add x y) z)
        `for`
            tuple3(intVector2, intVector2, intVector2)


addition_identity : Claim
addition_identity =
    claim "the zero vector is the identity element of vector addition"
        `that`
            (\x -> Vec2.add x { x = 0, y = 0 })
        `is`
            identity
        `for`
            floatVector2


floatVector2 : Producer (Vec2 Float)
floatVector2 =
    Check.Producer.map (\(x,y) -> { x = x, y = y }) (tuple (float, float))


intVector2 : Producer (Vec2 Int)
intVector2 =
    Check.Producer.map (\(x,y) -> { x = x, y = y }) (tuple (int, int))



-- Example tests


exampleTests : Test
exampleTests =
    suite "Examples"
        [ test "addition" <| assertEqual { x = 6, y = 6 } (Vec2.add { x = 2, y = -2 } { x = 4, y = 8 })
        ]
