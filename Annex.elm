module Annex exposing (..)

import  List

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

enumerate: List a -> List (Int, a)
enumerate list = zip (List.range 0 (List.length list)) list

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