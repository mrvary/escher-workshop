module Box exposing (Box, flipBox, moveVertically, scaleVertically, splitVertically, tossBox, turnBox)

import Vector exposing (..)


type alias Box =
    { a : Vector
    , b : Vector
    , c : Vector
    }



-- Exercise 1


turnBox : Box -> Box
turnBox { a, b, c } =
    { a = add a b
    , b = c
    , c = neg b
    }


flipBox : Box -> Box
flipBox { a, b, c } =
    { a = add a b
    , b = neg b
    , c = c
    }


tossBox : Box -> Box
tossBox { a, b, c } =
    { a =
        add b c
            |> add a
            |> scale 0.5
    , b =
        add b c
            |> scale 0.5
    , c =
        sub c b
            |> scale 0.5
    }


moveVertically : Float -> Box -> Box
moveVertically f { a, b, c } =
    { a = add a (scale f c)
    , b = b
    , c = c
    }


scaleVertically : Float -> Box -> Box
scaleVertically f { a, b, c } =
    { a = a
    , b = b
    , c = scale f c
    }


splitVertically : Float -> Box -> ( Box, Box )
splitVertically f box =
    let
        top =
            box |> moveVertically (1 - f) |> scaleVertically f

        bot =
            box |> scaleVertically (1 - f)
    in
    ( top, bot )
