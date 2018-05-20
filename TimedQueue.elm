module TimedQueue exposing (..)

import Queue
import Time exposing (Time)
import Tuple
import Annex


type alias QueueEntry a = (a, Time)

getItem : QueueEntry a -> a
getItem = Tuple.first

getDelay : QueueEntry a -> Time
getDelay = Tuple.second


type TimedQueue a = TQ (Queue.Queue (QueueEntry a)) Time

newTimedQueue : TimedQueue a
newTimedQueue = TQ Queue.newQueue 0

update : TimedQueue a -> Float -> TimedQueue a
update (TQ q currentTime) timeElapsed =
  (TQ q (currentTime + timeElapsed))


nextDelay : TimedQueue a -> Maybe Time
nextDelay (TQ q currentTime) =
  Queue.peek q 
    |> Tuple.first -- the head value.
    |> Annex.maybeChain getDelay


enqueue : TimedQueue a -> a -> Time -> TimedQueue a
enqueue (TQ q t) x delay =
  (TQ (Queue.enqueue (x, delay) q) t)


dequeue : TimedQueue a -> (Maybe a, TimedQueue a)
dequeue timedQueue =
  -- 1. nextDelay
  -- 2. "compareDelay" delay > timeElapsed => Nothing
  -- 3. dequeue
  -- 4. 'item' from entry
  let 
    (TQ q timeElapsed) = timedQueue
    delay = (nextDelay timedQueue)
  in
    case delay of
      Nothing -> (Nothing, timedQueue)
      (Just t) ->
        if t > timeElapsed then
          (Nothing, timedQueue)
        else
          let
            (entry, newQueue) = (Queue.dequeue q)
          in
            case entry of
              Nothing -> 
                (Nothing, (TQ newQueue timeElapsed))

              (Just (x, delay)) -> 
                ((Just x), (TQ newQueue timeElapsed))
