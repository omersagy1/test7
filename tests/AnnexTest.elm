module AnnexTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Annex exposing (..)

suite : Test
suite =
  describe "Annex Test"
  [
    test "Float range to zero."
      (\_ ->
        let
          l = rangeToZero 6 4
        in
          Expect.equal [6, 4, 2, 0] l)
  ,
    test "folding mutate"
      (\_ ->
        let
          state = 0
          doubler = (\x s -> (x * 2, s + 1))
          args = [1, 2, 3, 4, 5]
        in
          Expect.equal ([2, 4, 6, 8, 10], 5)
                       (foldingMutate doubler args state))
  ,
    test "folding mutate with string state"
      (\_ ->
        let
          state = ""
          stringifier = (\x s -> (toString x, s ++ "a"))
          args = [1, 2, 3, 4, 5]
        in
          Expect.equal (["1", "2", "3", "4", "5"], "aaaaa")
                       (foldingMutate stringifier args state))
  ,
    test "indexing"
      (\_ ->
        Expect.equal (Just "b") (["a", "b", "c", "d", "e"] !! 1))
  ,
    test "indexing end of list"
      (\_ ->
        Expect.equal (Just "e") (["a", "b", "c", "d", "e"] !! 4)
      )
  ,
    test "indexing beginning of list"
      (\_ ->
        Expect.equal (Just "a") (["a", "b", "c", "d", "e"] !! 0)
      )
  ]
