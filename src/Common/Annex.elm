module Common.Annex exposing (..)

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


ignoreResult : (s -> (a, s)) -> (s -> s)
ignoreResult f = (\x -> (f x) |> Tuple.second)


isJust : Maybe a -> Bool
isJust m =
  case m of
    Just x -> True
    other -> False


isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Nothing -> True
    other -> False


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


-- TODO: figure out how to use this idea properly.
maybePerform : (a -> b -> b) -> Maybe a -> b -> b
maybePerform mutateFn maybeArg struct =
  case maybeArg of
    Nothing -> struct
    Just arg -> mutateFn arg struct


(!!) : List a -> Int -> Maybe a
(!!) l i = List.take (i+1) l 
           |> List.drop i
           |> List.head


-- Apply a series of mutations to a struct, and collect the
-- extra results of those mutations in a list.
foldingMutate : (a -> c -> (b, c)) -> List a -> c -> (List b, c)
foldingMutate mutateFn argList struct =
  case argList of
    [] -> ([], struct)
    first::rest ->
      let 
        (result, nextStruct) = mutateFn first struct
        (restResults, finalStruct) = 
          foldingMutate mutateFn rest nextStruct
      in
        (result::restResults, finalStruct)


filterMutate : (a -> b -> (Bool, b)) -> List a -> b -> (List a, b)
filterMutate filterFn argList struct =
  case argList of
    [] -> ([], struct)
    first::rest ->
      let 
        (success, nextStruct) = filterFn first struct
        (restResults, finalStruct) = filterMutate filterFn rest nextStruct
      in
        if success then
          (first::restResults, finalStruct)
        else
          (restResults, finalStruct)


-- Mutate two data structures at once by folding over
-- a list of arguments. The mutating function has one
-- argument (besides the two data structures).
doubleFold : (a -> b -> c -> (b, c)) -> List a -> b -> c -> (b, c)
doubleFold mutateFn argList struct1 struct2 =
  case argList of
    [] -> (struct1, struct2)
    first::rest ->
      let
        (s1, s2) = mutateFn first struct1 struct2
      in
        doubleFold mutateFn rest s1 s2


-- Function composition (g . f) where 'f' takes
-- two arguments and 'g' takes one.
(>>>) : (a -> b -> c) -> (c -> d) -> a -> b -> d
(>>>) f g x y = (f x y) |> g