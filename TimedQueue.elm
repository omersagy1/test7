module TimedQueue exposing (..)

type alias Queue a = (List a, List a)

initial : Queue a
initial = ([], [])

enqueue : Queue a -> a -> Queue a
enqueue (inqueue, outqueue) i =
  (i :: inqueue, outqueue)

