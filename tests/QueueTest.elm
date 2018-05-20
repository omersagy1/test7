module QueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Queue exposing (..)

suite : Test
suite =
  describe "Vanilla Queue Tests"
  [
    test "one element dequeue"
      (\_ -> 
        let 
          q = newQueue
            |> (enqueue 9)
            |> (enqueue 5)
            |> (enqueue 2)
          e1 = dequeue q |> Tuple.first
        in
          Expect.equal (Just 9) e1)
  ,
    test "size of three-element queue"
      (\_ ->
        let 
          q = newQueue
           |> (enqueue 3)
           |> (enqueue 5)
           |> (enqueue 2)
        in
          Expect.equal 3 (size q))
  , 
    test "dequeue entire queue"
      (\_ -> 
        let 
          q = newQueue
           |> (enqueue 9)
           |> (enqueue 5)
           |> (enqueue 2)
           |> (ignoreResult dequeue)
           |> (ignoreResult dequeue)
           |> (ignoreResult dequeue)
           |> (enqueue 12)
        in
          Expect.equal 1 (size q))
  ,
    test "peek a queue returns the right head"
      (\_ ->
        let
          q = newQueue
            |> (enqueue 2)
            |> (enqueue 8)
          e1 = peek q |> Tuple.first
        in
          Expect.equal (Just 2) e1)
  ,
    test "peeking queue does not affect size"
      (\_ ->
        let
          q = newQueue
            |> (enqueue 2)
            |> (enqueue 8)
            |> (ignoreResult peek)
        in
          Expect.equal 2 (size q))
  ,
    test "use pipes"
      (\_ ->
        let q = newQueue
                  |> (enqueue 1)
                  |> (enqueue 2)
                  |> (enqueue 5)
                  |> (ignoreResult dequeue)
                  |> (ignoreResult dequeue)
        in
          Expect.equal 1 (size q))
  ]
