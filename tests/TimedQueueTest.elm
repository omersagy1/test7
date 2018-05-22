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
    test "enqueue one item"
      (\_ -> 
        let 
          tq = new |> (enqueue "a" (3*Time.second))
        in
          Expect.equal 1 (size tq))
  ,
    test "enqueue multiple items"
      (\_ -> 
        let 
          tq = new 
            |> (enqueue "a" (3*Time.second))
            |> (enqueue "b" (2*Time.second))
            |> (enqueue "c" (5*Time.second))
        in
          Expect.equal 3 (size tq))
  ,
    test "get the next delay"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (2*Time.second))
            |> (enqueue "b" (5*Time.second))
        in
          Expect.equal (Just (2*Time.second)) 
                       (nextDelay tq))
  ,
    test "can dequeue"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (2*Time.second))
            |> (enqueue "b" (5*Time.second))
            |> (update (3*Time.second))
        in
          Expect.equal True (canDequeue tq))
  ,
    test "can't dequeue"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (3*Time.second))
            |> (enqueue "b" (5*Time.second))
            |> (update (2*Time.second))
        in
          Expect.equal False (canDequeue tq))
  ]