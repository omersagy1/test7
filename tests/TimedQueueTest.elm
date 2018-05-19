module TimedQueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import TimedQueue exposing (Queue)

suite : Test
suite =
  test "enqueue"
    (\_ -> 
      let 
        q1= (TimedQueue.enqueue TimedQueue.initial 3)
        q2 = (TimedQueue.enqueue q1 5)
        q3 = (TimedQueue.enqueue q2 2)
      in
        Expect.equal ([2, 5, 3], []) q3)