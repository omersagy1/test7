module QueueTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Queue exposing (Queue)

suite : Test
suite =
  describe "Vanilla Queue Tests"
  [
    test "one element dequeue"
      (\_ -> 
        let 
          q1 = (Queue.enqueue Queue.newQueue 9)
          q2 = (Queue.enqueue q1 5)
          q3 = (Queue.enqueue q2 2)
          (e1, q4) = (Queue.dequeue q3)
        in
          Expect.equal e1 (Just 9))
  ,
    test "size of three-element queue"
      (\_ ->
        let 
          q1= (Queue.enqueue Queue.newQueue 3)
          q2 = (Queue.enqueue q1 5)
          q3 = (Queue.enqueue q2 2)
        in
          Expect.equal (Queue.size q3) 3)
  , 
    test "dequeue entire queue"
      (\_ -> 
        let 
          q1 = (Queue.enqueue Queue.newQueue 9)
          q2 = (Queue.enqueue q1 5)
          q3 = (Queue.enqueue q2 2)
          (e1, q4) = (Queue.dequeue q3)
          (e2, q5) = (Queue.dequeue q4)
          (e3, q6) = (Queue.dequeue q5)
          q7 = (Queue.enqueue q6 12)
        in
          Expect.equal (Queue.size q7) 1)
  ]
