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
  ]
