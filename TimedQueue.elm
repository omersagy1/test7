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
    res, _ = Queue.peek q
  in
    case res of
      Nothing -> Nothing
      (Just (x, delay)) -> delay


enqueue : TimedQueue a -> a -> Time -> TimedQueue a
enqueue (TQ q t) x delay =
  (TQ (Queue.enqueue q (x, delay)) t)


canDequeue : TimedQueue a -> Bool
canDequeue (TQ q timeElapsed) = 
  case (nextDelay (TQ q timeElapsed)) of
    Nothing -> False
    Just delay -> delay > timeElapsed


dequeue : TimedQueue a -> (Maybe a, TimedQueue a)
dequeue (TQ q timeElapsed) =
  if not (Queue.canDequeue q) then
    (Nothing, (TQ q timeElapsed))
  else
    let
      (x, newQueue) = (Queue.dequeue q)
    in
      (x, (TQ newQueue timeElapsed))
