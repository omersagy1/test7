module TimedQueue exposing (..)

import Queue

type TimedQueue a = TQ (Queue (QueueEntry a)) Float

type QueueEntry a = (a, Float)

newTimedQueue : TimedQueue a
newTimedQueue = TQ Queue.newQueue 0

update : TimedQueue a -> Float -> TimedQueue a
update (TQ q currentTime) timeElapsed =
  (TQ q currentTime + timeElapsed)

canDequeue : TimedQueue a -> Boolean
