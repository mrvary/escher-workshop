module Picture exposing (Picture, Rendering, above, aboveRatio, beside, besideExtra, besideRatio, besideRatioExtra, blank, corner, flip, nonet, over, overall, quartet, side, squareLimit, times, toss, ttile, turn, turnUpsideDown, turns, utile)

import Box exposing (..)
import Shape exposing (..)
import Style exposing (..)


type alias Rendering =
    List ( Shape, Style )


type alias Picture =
    Box -> Rendering


blank : Picture
blank _ =
    []



-- Exercise 1


turn : Picture -> Picture
turn p =
    turnBox >> p



-- Entirely optional bonus exercise:


times : Int -> (a -> a) -> (a -> a)
times n fn =
    if n == 0 then
        identity

    else
        fn >> times (n - 1) fn


turns : Int -> Picture -> Picture
turns n p =
    times n turn p



-- Exercise 2


flip : Picture -> Picture
flip p =
    flipBox >> p



-- Exercise 3


toss : Picture -> Picture
toss p =
    tossBox >> p



-- Exercise 4


aboveRatio : Int -> Int -> Picture -> Picture -> Picture
aboveRatio m n p1 p2 =
    \box ->
        let
            f =
                toFloat m / toFloat (m + n)

            ( b1, b2 ) =
                splitVertically f box
        in
        p1 b1 ++ p2 b2


above : Picture -> Picture -> Picture
above p1 p2 =
    aboveRatio 1 1 p1 p2



-- Exercise 5


besideRatio : Int -> Int -> Picture -> Picture -> Picture
besideRatio m n p1 p2 =
    \box ->
        let
            f =
                toFloat m / toFloat (m + n)

            ( b1, b2 ) =
                splitHorizontally f box
        in
        p1 b1 ++ p2 b2


besideRatioExtra : Int -> Int -> Picture -> Picture -> Picture
besideRatioExtra m n p1 p2 =
    turn (aboveRatio 1 1 (turns 3 p1) (turns 3 p2))


beside : Picture -> Picture -> Picture
beside =
    besideRatio 1 1


besideExtra : Picture -> Picture -> Picture
besideExtra =
    besideRatioExtra 1 1



-- Exercise 6


turnUpsideDown : Picture -> Picture
turnUpsideDown p =
    turns 2 p


quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se =
    beside (above nw sw) (above ne se)



-- Exercise 7


nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se =
    let
        row w m e =
            besideRatio 1 2 w (beside m e)

        column n m s =
            aboveRatio 1 2 n (above m s)
    in
    column (row nw nm ne) (row mw mm me) (row sw sm se)



-- Exercise 8


over : Picture -> Picture -> Picture
over p1 p2 =
    \box -> p1 box ++ p2 box


overall : List Picture -> Picture
overall ps =
    \box -> List.concatMap (\p -> p box) ps



-- Exercise 9


ttile : Picture -> Picture
ttile fish =
    overall
        [ fish
        , fish |> toss |> flip
        , fish |> toss |> flip |> times 3 turn
        ]



-- Exercise 10


utile : Picture -> Picture
utile fish =
    overall
        [ fish |> toss |> flip
        , fish |> toss |> flip |> times 3 turn
        , fish |> toss |> flip |> turn
        , fish |> toss |> flip |> times 2 turn
        ]



-- Exercise 11


side : Int -> Picture -> Picture
side n fish =
    if n == 0 then
        blank

    else
        quartet
            (side (n - 1) fish)
            (side (n - 1) fish)
            (fish |> ttile |> turn)
            (ttile fish)



-- Exercise 12


corner : Int -> Picture -> Picture
corner n fish =
    if n == 0 then
        blank

    else
        quartet
            (corner (n - 1) fish)
            (side (n - 1) fish)
            (fish |> side (n - 1) |> turn)
            (utile fish)



-- Exercise 13


squareLimit : Int -> Picture -> Picture
squareLimit n fish =
    nonet
        (corner n fish)
        (side n fish)
        (corner n fish |> times 3 turn)
        (side n fish |> turn)
        (utile fish)
        (side n fish |> times 3 turn)
        (corner n fish |> turn)
        (side n fish |> times 2 turn)
        (corner n fish |> times 2 turn)
