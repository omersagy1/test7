module TimedQueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time

import TimedQueue exposing (TimedQueue)

suite : Test
suite =
  describe "Timed Queue Tests"
  [
    test "dequeue one item"
      (\_ ->
        let
          q1 = TimedQueue.newTimedQueue
          q2 = (TimedQueue.enqueue q1 1 (2 * Time.second))
          q3 = (TimedQueue.update q2 (2 * Time.second))
          (e1, q4) = (TimedQueue.dequeue q3)
        in
          Expect.equal (Just 1) e1)
  ]