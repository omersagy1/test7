module TimedQueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time

import TimedQueue exposing (TimedQueue)

suite : Test
suite =
  describe "Vanilla Queue Tests"
  [
    test "dequeue one item"
      (\_ ->
        let
          q1 = TimedQueue.newTimedQueue
          q2 = (TimedQueue.enqueue 1 (2 * second))
          q3 = (TimedQueue.update q2 (2 * second))
        in
          Expect.equal 1 (TimedQueue.dequeue q3))

  ]