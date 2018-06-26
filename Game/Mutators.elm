module Game.Mutators exposing (..)

import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource


type alias Mutator = GameState -> GameState


addToResource : String -> Int -> Mutator
addToResource name x = 
  (\s ->
    let stateWithActiveResource =
      if not (GameState.resourceActive name s) then
        GameState.applyToResource name (Resource.activateWithCooldown) s
      else
        s
    in
      GameState.applyToResource 
        name (Resource.add x) stateWithActiveResource)


subtractResource : String -> Int -> Mutator
subtractResource name x = 
  GameState.applyToResource name (Resource.subtract x)


setResourceAmount : String -> Int -> Mutator
setResourceAmount name x = 
  GameState.applyToResource name (Resource.mutate (\_ -> x))


and : Mutator -> Mutator -> Mutator
and m1 m2 = (\s -> s |> m1 |> m2)


setMilestone : String -> Mutator
setMilestone name =
  (\s -> GameState.setMilestoneReached name s)