module Annex exposing (..)

import  List

-- This module is for generic utility functions
-- not found in the Elm standard library.

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)


enumerate: List a -> List (Int, a)
enumerate list = zip (List.range 0 (List.length list)) list


-- e.g. 4.0 -> 6 -> [4.0, 3.2, 2.4, 1.6, .8, 0]
rangeToZero : Float -> Int -> List Float
rangeToZero initial len =
  let step = initial / ((toFloat len) - 1)
  in
    List.map (\x -> initial - step * toFloat(x)) (List.range 0 (len - 1))


-- Given a function guarateed to produce a value
-- and an argument that might exist, produce a
-- result that might exist.
maybeChain : (a -> b) -> Maybe a -> Maybe b
maybeChain callback maybe =
  case maybe of
    Nothing -> Nothing
    Just value ->
      Just (callback value)


ignoreResult : (s -> (a, s)) -> (s -> s)
ignoreResult f = (\x -> (f x) |> Tuple.second)


isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    (Just x) -> True


maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Nothing -> []
    Just x -> [x]


concatMaybes : List (Maybe a) -> List a
concatMaybes l =
  List.map maybeToList l
  |> List.concat
      

maybePred : (a -> Bool) -> Maybe a -> Bool
maybePred pred m =
  case m of
    Nothing -> False
    Just x -> pred x