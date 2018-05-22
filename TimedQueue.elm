module TimedQueue exposing (..)

import Queue
import Time exposing (Time)
import Tuple
import Annex


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


enqueue : a -> Time -> TimedQueue a -> TimedQueue a
enqueue item delay timedQueue =
  { timedQueue | queue = (Queue.enqueue 
                            { item = item, delay = delay }
                            timedQueue.queue) 
  }

size : TimedQueue a -> Int
size timedQueue = (Queue.size timedQueue.queue)