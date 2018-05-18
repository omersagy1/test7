module Annex exposing (..)

import  List

double : Float -> Float
double = (*) 2

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

enumerate: List a -> List (Int, a)
enumerate list = zip (List.range 0 (List.length list)) list
