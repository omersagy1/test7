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
        case remainingFront of
          Just x -> (rtn, (Q x []))
          Nothing -> (rtn, (Q [] []))

    -- Front is non-empty, return its first element.
    (Q (x::xs) rear) -> (Just x, (Q xs rear))

size : Queue a -> Int
size (Q front rear) = (List.length front) + (List.length rear)
