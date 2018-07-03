module TimedQueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Time exposing (second)
import Tuple exposing (..)

import Common.Annex exposing (..)
import Queue.TimedQueue exposing (..)


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
  ,
    test "dequeue one element"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (3*Time.second))
            |> (enqueue "b" (5*Time.second))
            |> (update (4*Time.second))
          e1 = (dequeue tq) |> first
        in
          Expect.equal (Just "a") e1)
  ,
    test "delay only counts for one element"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (3*Time.second))
            |> (enqueue "b" (5*Time.second))
            |> (update (10*Time.second))
            |> (ignoreResult dequeue)
        in
          Expect.equal Nothing (dequeue tq |> first))
  ,
    test "dequeue entire queue"
      (\_ ->
        let 
          tq = new
            |> (enqueue "a" (3*Time.second))
            |> (enqueue "b" (5*Time.second))
            |> (enqueue "c" (2*Time.second))
            |> (update (4*Time.second))
            |> (ignoreResult dequeue)
            |> (update (5*Time.second))
            |> (ignoreResult dequeue)
            |> (update (3*Time.second))
            |> (enqueue "d" (8*Time.second))
            |> (ignoreResult dequeue)
            |> (update (9*Time.second))
            |> (ignoreResult dequeue)
        in
          Expect.equal 0 (size tq))
  ]