module Queue.Queue exposing (..)

import Tuple

-- QUEUE type
type Queue a = Q (List a) (List a)

newQueue : Queue a
newQueue = Q [] []

enqueue : a -> Queue a -> Queue a
enqueue x (Q front rear) = Q front (x::rear)

dequeue : Queue a -> (Maybe a, Queue a)
dequeue queue =
  case queue of
    -- Queue is empty, return nothing.
    Q [] [] -> (Nothing, Q [] [])

    -- Front is empty, reverse the rear and return the first element.
    Q [] rear -> 
      let 
        front = List.reverse rear
        rtn = List.head front
        remainingFront = List.tail front
      in
        -- TODO: This check should not be necessary since we know
        -- that 'rear' is not [].
        case remainingFront of
          Nothing -> (rtn, Q [] [])
          Just x -> (rtn, Q x [])

    -- Front is non-empty, return its first element.
    Q (x::xs) rear -> (Just x, Q xs rear)


peek : Queue a -> Maybe a
peek queue =
  case queue of
    Q [] [] -> Nothing
    Q [] rear -> List.reverse rear |> List.head
    Q front rear -> List.head front


size : Queue a -> Int
size (Q front rear) = (List.length front) + (List.length rear)
