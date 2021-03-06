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


fish =
    createPicture fishShapes


g =
    createPicture george


f =
    createPicture fLetter


e =
    createPicture eLetter


h =
    createPicture hLetter


n =
    createPicture nLetter


d =
    createPicture dLetter


r =
    createPicture rLetter


s =
    createPicture sLetter


o =
    createPicture oLetter


quartetGeorge =
    let
        ne =
            flip (turnUpsideDown g)

        se =
            flip g

        sw =
            turnUpsideDown g
    in
    quartet g ne sw se


quartetComposition =
    let
        inner =
            quartet quartetGeorge blank blank quartetGeorge
    in
    quartet inner inner inner inner


main : Svg msg
main =
    let
        box =
            { a = { x = 125.0, y = 75.0 }
            , b = { x = 250.0, y = 0.0 }
            , c = { x = 0.0, y = 250.0 }
            }

        zoom p =
            nonet h e n d p r s o n
    in
    box
        |> squareLimit 5 fish
        |> toSvgWithBoxes ( 500, 500 ) []
        |> placeInsideDiv
