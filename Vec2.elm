module Vec2 exposing
    ( Vec2
    , add
    , minus
    , scale
    , dot
    , length
    , normalize
    , clamp
    , toFloat
    )

type alias Vec2 a =
    { x : a
    , y : a
    }


add : Vec2 number -> Vec2 number -> Vec2 number
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


minus : Vec2 number -> Vec2 number -> Vec2 number
minus v1 v2 =
    v1 `add` (scale -1 v2)


scale : number -> Vec2 number -> Vec2 number
scale scalar vector =
    { x = scalar * vector.x
    , y = scalar * vector.y
    }


dot : Vec2 number -> Vec2 number -> number
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


clamp : Vec2 number -> Vec2 number -> Vec2 number -> Vec2 number
clamp min max vec =
    { x = Basics.clamp min.x max.x vec.x
    , y = Basics.clamp min.y max.y vec.y
    }


length : Vec2 Float -> Float
length v =
    sqrt (v.x ^ 2 + v.y ^ 2)


normalize : Vec2 Float -> Vec2 Float
normalize v =
    let
        l = length v
    in
        { x = v.x / l
        , y = v.y / l
        }


toFloat : Vec2 Int -> Vec2 Float
toFloat v =
    { x = Basics.toFloat v.x
    , y = Basics.toFloat v.y
    }
