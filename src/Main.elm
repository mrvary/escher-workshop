module Main exposing (main)

import Box exposing (..)
import Figure exposing (george)
import Fishy exposing (fishShapes)
import Fitting exposing (createPicture)
import Html exposing (..)
import Html.Attributes exposing (..)
import Letter exposing (..)
import Picture exposing (..)
import Rendering exposing (..)
import Svg exposing (Svg)


placeInsideDiv : Svg msg -> Html msg
placeInsideDiv svg =
    div [ style "padding" "50px" ] [ svg ]


main : Svg msg
main =
    let
        box =
            { a = { x = 125.0, y = 75.0 }
            , b = { x = 250.0, y = 0.0 }
            , c = { x = 0.0, y = 250.0 }
            }

        f =
            createPicture fLetter
    in
    box
        |> above (besideExtra f f) (beside f f)
        |> toSvgWithBoxes ( 500, 500 ) []
        |> placeInsideDiv
