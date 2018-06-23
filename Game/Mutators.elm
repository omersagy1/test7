module Game.Mutators exposing (..)

import Game.GameState as GameState exposing (GameState)


type alias Mutator = GameState -> GameState

mutateResource : String -> (Int -> Int) -> Mutator
mutateResource = GameState.mutateResource


addToResource : String -> Int -> Mutator
addToResource name x = 
  (\s ->
    let stateWithActiveResource =
      if not (GameState.resourceActive name s) then
        GameState.activateResource name s
      else
        s
    in
      GameState.mutateResource name ((+) x) stateWithActiveResource)


subtractResource : String -> Int -> Mutator
subtractResource name x = GameState.mutateResource name (\y -> y - x)


setResourceAmount : String -> Int -> Mutator
setResourceAmount name x = GameState.mutateResource name (\_ -> x)


and : Mutator -> Mutator -> Mutator
and m1 m2 = (\s -> s |> m1 |> m2)
