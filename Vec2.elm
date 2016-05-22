module Vec2 exposing
    ( Vec2
    , add
    , subtract
    , scale
    , dot
    , length
    , normalize
    , clamp
    )

type alias Vec2 =
    { x : Float
    , y : Float
    }


add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }


subtract : Vec2 -> Vec2 -> Vec2
subtract v1 v2 =
    v1 `add` (scale -1 v2)


scale : number -> Vec2 -> Vec2
scale scalar vector =
    { x = scalar * vector.x
    , y = scalar * vector.y
    }


dot : Vec2 -> Vec2 -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y


clamp : Vec2 -> Vec2 -> Vec2 -> Vec2
clamp min max vec =
    { x = Basics.clamp min.x max.x vec.x
    , y = Basics.clamp min.y max.y vec.y
    }


length : Vec2 -> Float
length v =
    sqrt (v.x ^ 2 + v.y ^ 2)


normalize : Vec2 -> Vec2
normalize v =
    let
        l = length v
    in
        { x = v.x / l
        , y = v.y / l
        }
