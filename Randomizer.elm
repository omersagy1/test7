module Randomizer exposing (..)

import Time exposing (Time)
import Random exposing (Seed, Generator)

import Annex exposing (..)


type alias Randomizer = 
  { seed : Seed
  } 


init : Time -> Randomizer
init t =
  { seed = Random.initialSeed (round t)
  }


choose : List a -> Randomizer -> (Maybe a, Randomizer)
choose list randomizer =
  let
    (index, newSeed) = Random.step (fromList list) randomizer.seed
  in
    ( list !! index
    , { randomizer | seed = newSeed }
    )


fromList : List a -> Generator Int
fromList l = Random.int 0 ((List.length l) - 1)