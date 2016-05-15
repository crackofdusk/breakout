module Vec2 exposing (..)

type alias Vec2 a =
    { x : a
    , y : a
    }


add : Vec2 number -> Vec2 number -> Vec2 number
add v1 v2 =
    { x = v1.x + v2.x
    , y = v1.y + v2.y
    }
