module Queue.TimedQueue exposing (..)

import Time exposing (Time)

import Common.Annex exposing (..)
import Queue.Queue as Queue


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

push : a -> Time -> TimedQueue a -> TimedQueue a
push item delay timedQueue =
  { timedQueue | queue = (Queue.push 
                            { item = item, delay = delay }
                            timedQueue.queue) 
  }

size : TimedQueue a -> Int
size timedQueue = (Queue.size timedQueue.queue)

nextDelay : TimedQueue a -> Maybe Time
nextDelay timedQueue =
  Queue.peek timedQueue.queue 
  |> Maybe.map .delay

canDequeue : TimedQueue a -> Bool
canDequeue timedQueue =
  nextDelay timedQueue
  |> Maybe.map ((>=) timedQueue.currentTime)
  |> (==) (Just True)

dequeue : TimedQueue a -> (Maybe a, TimedQueue a)
dequeue timedQueue =
  if (canDequeue timedQueue) then
    let 
      (entry, q) = (Queue.dequeue timedQueue.queue)
    in
      (entry |> Maybe.map .item, 
       { timedQueue | queue = q , currentTime = 0 })
  else
    (Nothing, timedQueue)