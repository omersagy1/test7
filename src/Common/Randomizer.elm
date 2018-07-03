module Common.Randomizer exposing (..)

import Time exposing (Time)
import Random exposing (Seed, Generator)

import Common.Annex exposing (..)


type alias Randomizer = Seed


init : Time -> Randomizer
init t = 
  Random.initialSeed (round t)
  |> churn


-- Generate several random numbers.
-- For superstitious users.
churn : Randomizer -> Randomizer
churn randomizer =
  foldingMutate
    (\upper r -> int 0 upper r)
    (List.range 0 100)
    randomizer
  |> Tuple.second


int : Int -> Int -> Randomizer -> (Int, Randomizer)
int lower upper randomizer = 
  Random.step (Random.int lower upper) randomizer


choose : List a -> Randomizer -> (Maybe a, Randomizer)
choose list randomizer =
  let
    (index, newRandomizer) = Random.step (fromList list) randomizer
  in
    (list !! index, newRandomizer)


fromList : List a -> Generator Int
fromList l = Random.int 0 ((List.length l) - 1)