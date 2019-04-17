module Box exposing (..)

import Vector exposing (..)

type alias Box = 
  { a : Vector
  , b : Vector
  , c : Vector }

-- Exercise 1 

turnBox : Box -> Box 
turnBox { a, b, c } = 
  { a = add a b 
  , b = c 
  , c = neg b }

flipBox : Box -> Box
flipBox { a, b, c } =
  { a = add a b 
  , b = neg b 
  , c = c }

tossBox : Box -> Box
tossBox { a, b, c } =
  { a = add b c
          |> add a
          |> scale 0.5
  , b = add b c
          |> scale 0.5
  , c = sub c b 
          |> scale 0.5
  }
