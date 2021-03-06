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
    let
        fishN =
            fish |> toss |> flip

        fishW =
            turn fishN

        fishS =
            turn fishW

        fishE =
            turn fishS
    in
    overall
        [ fishN, fishW, fishS, fishE ]



-- Exercise 11


side : Int -> Picture -> Picture
side n fish =
    if n == 0 then
        blank

    else
        let
            s =
                side (n - 1) fish

            t =
                ttile fish
        in
        quartet s s (turn t) t



-- Exercise 12


corner : Int -> Picture -> Picture
corner n fish =
    if n == 0 then
        blank

    else
        let
            c =
                corner (n - 1) fish

            s =
                side (n - 1) fish
        in
        quartet c s (turn s) (utile fish)



-- Exercise 13


squareLimit : Int -> Picture -> Picture
squareLimit n fish =
    let
        c =
            corner n fish

        s =
            side n fish

        nw =
            c

        nm =
            s

        ne =
            c |> turns 3

        mw =
            s |> turn

        mm =
            utile fish

        me =
            s |> turns 3

        sw =
            c |> turn

        sm =
            s |> turns 2

        se =
            c |> turns 2
    in
    nonet nw nm ne mw mm me sw sm se
