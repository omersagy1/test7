module Queue exposing (..)

-- QUEUE type
type Queue a = Q (List a) (List a)

newQueue : Queue a
newQueue = (Q [] [])

enqueue : Queue a -> a -> Queue a
enqueue (Q front rear) i = (Q front (i::rear))

dequeue : Queue a -> (Maybe a, Queue a)
dequeue queue =
  case queue of
    -- Queue is empty, return nothing.
    (Q [] []) -> (Nothing, (Q [] []))

    -- Front is empty, reverse the rear and return the first element.
    (Q [] rear) -> 
      let 
        front = List.reverse rear
        rtn = List.head front
        remainingFront = List.tail front
      in
        -- TODO: This check should not be necessary since we know
        -- that 'rear' is not [].
        case remainingFront of
          Just x -> (rtn, (Q x []))
          Nothing -> (rtn, (Q [] []))

    -- Front is non-empty, return its first element.
    (Q (x::xs) rear) -> (Just x, (Q xs rear))

-- Can transform the internal representation
-- of the queue, altough conceptually it does
-- not change. This prevents us from reversing
-- twice in the case we 'peek' a queue with
-- an empty 'front' and then immediately 'dequeue'.
peek : Queue a -> (Maybe a, Queue a)
peek queue =
  let 
    (rtn, q2) = dequeue queue
    q3 = case rtn of
          Nothing -> queue
          (Just x) -> enqueue q2 x
  in
    (rtn, q3)

size : Queue a -> Int
size (Q front rear) = (List.length front) + (List.length rear)

canDequeue : Queue a -> Bool
canDequeue q = size q > 0