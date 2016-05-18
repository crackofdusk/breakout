module Collision exposing
    ( BoundingBox
    , Direction(..)
    , Circle
    , circleVsAABB
    , reboundDirection
    )


import Vec2 exposing (Vec2)


type alias Collision =
    { isColliding : Bool
    , penetration : Vec2 Int
    --, normal : Vec2 Int
    }


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Circle =
    { center : Vec2 Int
    , radius : Int
    }


type alias BoundingBox =
    { topLeft : Vec2 Int
    , bottomRight : Vec2 Int
    }


circleVsAABB : Circle -> BoundingBox -> Collision
circleVsAABB circle rectangle =
    let
        center = boxCenter rectangle
        halfExtents = boxHalfExtents rectangle
        difference = circle.center `Vec2.minus` center
        clamped = Vec2.clamp (Vec2.scale -1 halfExtents) halfExtents difference
        closest = center `Vec2.add` clamped
        distance = Vec2.minus closest circle.center
        isColliding =
            Vec2.length (Vec2.toFloat distance) < toFloat circle.radius
        penetration =
            if isColliding then
                distance
            else
                { x = 0, y = 0 }
    in
        { isColliding = isColliding
        , penetration = penetration
        }


boxCenter : BoundingBox -> Vec2 Int
boxCenter box =
    let
        x1 = box.topLeft.x
        x2 = box.bottomRight.x
        y1 = box.topLeft.y
        y2 = box.bottomRight.y
    in
        { x = x1 + round (toFloat (x2 - x1) / 2)
        , y = y1 + round (toFloat (y2 - y1) / 2)
        }


boxHalfExtents : BoundingBox -> Vec2 Int
boxHalfExtents box =
    let
        x1 = box.topLeft.x
        x2 = box.bottomRight.x
        y1 = box.topLeft.y
        y2 = box.bottomRight.y
    in
        { x = round (toFloat (x2 - x1) / 2)
        , y = round (toFloat (y2 - y1) / 2)
        }


reboundDirection : Vec2 Int -> Direction
reboundDirection penetration =
    let
        normalizedPenetration =  Vec2.normalize (Vec2.toFloat penetration)
        dotProduct = Vec2.dot normalizedPenetration
        directions =
            [ (Up, dotProduct { x = 0, y = 1 })
            , (Right, dotProduct { x = 1, y = 0 })
            , (Down, dotProduct { x = 0, y = -1 })
            , (Left, dotProduct { x = -1, y = 0 })
            ]
        sorted = List.sortBy snd directions
    in
        case List.head sorted of
            Just (direction, product) ->
                direction

            _ ->
                Down

