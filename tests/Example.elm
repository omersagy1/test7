module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Annex


suite : Test
suite =

  describe "Basic Functionality"
  [

    test "does not crash" 
      (\_ -> Expect.equal 4 (2 + 2))

  , test "zip works" 
      (\_ -> Expect.equal (Annex.zip [1, 2] ["a", "b"])
                          [(1, "a"), (2, "b")])

  , test "enumerate works"
      (\_ -> Expect.equal (Annex.enumerate ["a", "b", "c"])
                          [(0, "a"), (1, "b"), (2, "c")])
  ]

