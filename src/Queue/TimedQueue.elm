module Queue.TimedQueue exposing (..)

import Queue.Queue as Queue
import Time exposing (Time)
import Annex exposing (..)


type alias QueueEntry a = 
  { item: a
  , delay: Time
  }


type alias TimedQueue a = 
  { queue: Queue.Queue (QueueEntry a)
  , currentTime: Time
  }


new : TimedQueue a
new = { queue = Queue.newQueue
      , currentTime = 0
      }

update : Time -> TimedQueue a -> TimedQueue a
update timeElapsed timedQueue =
  { timedQueue | currentTime = timedQueue.currentTime + timeElapsed }

enqueue : a -> Time -> TimedQueue a -> TimedQueue a
enqueue item delay timedQueue =
  { timedQueue | queue = (Queue.enqueue 
                            { item = item, delay = delay }
                            timedQueue.queue) 
  }

size : TimedQueue a -> Int
size timedQueue = (Queue.size timedQueue.queue)

nextDelay : TimedQueue a -> Maybe Time
nextDelay timedQueue =
  Queue.peek timedQueue.queue 
    |> maybeChain .delay

canDequeue : TimedQueue a -> Bool
canDequeue timedQueue =
  nextDelay timedQueue
    |> maybeChain ((>=) timedQueue.currentTime)
    |> (==) (Just True)

dequeue : TimedQueue a -> (Maybe a, TimedQueue a)
dequeue timedQueue =
  if (canDequeue timedQueue) then
    let 
      (entry, q) = (Queue.dequeue timedQueue.queue)
    in
      (entry |> maybeChain .item, { timedQueue | queue = q 
                                               , currentTime = 0 })
  else
    (Nothing, timedQueue)