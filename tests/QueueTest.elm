module QueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Queue exposing (..)
import Annex exposing (..)

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
    test "multiple dequeues"
      (\_ ->
        let q = newQueue
              |> (enqueue 99)
              |> (enqueue 2)
              |> (enqueue 5)
              |> (ignoreResult dequeue)
            e1 = dequeue q |> Tuple.first
        in
          Expect.equal (Just 2) e1)
  ,
    test "peek a queue returns the right head"
      (\_ ->
        let
          q = newQueue
            |> (enqueue 2)
            |> (enqueue 8)
          e1 = peek q
        in
          Expect.equal (Just 2) e1)
  ,
    test "alternate enqueue and dequeue"
      (\_ ->
        let q = newQueue
              |> (enqueue 1)
              |> (enqueue 2)
              |> (ignoreResult dequeue)
              |> (enqueue 3)
              |> (ignoreResult dequeue)
              |> (ignoreResult dequeue)
              |> (enqueue 4)
            e1 = dequeue q |> Tuple.first
        in
          Expect.equal (Just 4) e1)
  ]
