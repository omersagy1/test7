module TimedQueue exposing (..)

import Queue

type TimedQueue a = TQ (Queue (QueueEntry a)) Float

type QueueEntry a = (a, Float)

newTimedQueue : TimedQueue a
newTimedQueue = TQ Queue.newQueue 0

update : TimedQueue a -> Float -> TimedQueue a
update (TQ q currentTime) timeElapsed =
  (TQ q currentTime + timeElapsed)

nextDelay : TimedQueue a -> Maybe Float
nextDelay (TQ q currentTime) =
  if not (Queue.canDequeue q) then 
    Nothing
  else
    let 
      (x, delay) = Queue.peek q
    in
      Just delay

canDequeue : TimedQueue a -> Boolean
canDequeue (TQ q t) = 
  case (nextDelay (TQ q t)) of
    Nothing -> False
    Just t -> t
