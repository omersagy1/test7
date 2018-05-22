module TimedQueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import TimedQueue exposing (..)
import Annex exposing (..)
import Tuple exposing (..)
import Time exposing (second)

suite : Test
suite =
  describe "Timed Queue Tests"
  [
    test "enqueue works"
      (\_ -> 
        let 
          tq = new |> (enqueue "a" (3*Time.second))
        in
          Expect.equal 1 (size tq))
  ]