module Game.Mutators exposing (..)

import Game.Effect as Effect exposing (Effect)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource


type alias Mutator = GameState -> GameState


applyEffect : Effect -> GameState -> GameState
applyEffect e s =
  case e of
    Effect.ActivateResource name -> (activateResource name) s
    Effect.AddToResource name x -> (addToResource name x) s
    Effect.SubtractResource name x -> (subtractResource name x) s
    Effect.SetResourceAmount name x -> (setResourceAmount name x) s
    Effect.SetMilestoneReached name -> (setMilestoneReached name) s
    Effect.Compound effects -> List.foldl applyEffect s effects
    Effect.Compound2 e1 e2 -> List.foldl applyEffect s [e1, e2]


activateResource : String -> Mutator
activateResource name =
  (\s -> GameState.applyToResource name (Resource.activate) s)


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


setMilestoneReached : String -> Mutator
setMilestoneReached name =
  (\s -> GameState.setMilestoneReached name s)