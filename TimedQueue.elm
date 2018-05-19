module TimedQueue exposing (..)

import Queue
import Time exposing (Time)

type TimedQueue a = TQ (Queue.Queue (QueueEntry a)) Time

type alias QueueEntry a = (a, Time)

newTimedQueue : TimedQueue a
newTimedQueue = TQ Queue.newQueue 0

update : TimedQueue a -> Float -> TimedQueue a
update (TQ q currentTime) timeElapsed =
  (TQ q (currentTime + timeElapsed))

nextDelay : TimedQueue a -> Maybe Time
nextDelay (TQ q currentTime) =
  let 
    (res, _) = Queue.peek q
  in
    case res of
      Nothing -> Nothing
      (Just (x, delay)) -> (Just delay)


enqueue : TimedQueue a -> a -> Time -> TimedQueue a
enqueue (TQ q t) x delay =
  (TQ (Queue.enqueue q (x, delay)) t)


dequeue : TimedQueue a -> (Maybe a, TimedQueue a)
dequeue timedQueue =
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
