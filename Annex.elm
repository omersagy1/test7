module Annex exposing (..)

import  List

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

enumerate: List a -> List (Int, a)
enumerate list = zip (List.range 0 (List.length list)) list

-- Given a function guarateed to produce a value
-- and an argument that might exist, produce a
-- result that might exist.
andThen : (a -> b) -> Maybe a -> Maybe b
andThen callback maybe =
  case maybe of
    Nothing -> Nothing
    Just value ->
      Just (callback value)
