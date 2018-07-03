module Data.Init exposing (init)

import Time

import Game.Action as Action
import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource


init : GameState
init =
  GameState.init 
  |> GameState.initFire (30*Time.second) (10*Time.second)
  |> GameState.addResource (Resource.init "wood" 0)
  |> GameState.addResource (Resource.init "rats" 0)
  |> GameState.addResource (Resource.init "gold" 0)
  |> GameState.addCustomAction
      (Action.init "search for wood"
        |> Action.cooldown (30*Time.second)
        |> Action.effect (AddToResourceRand "wood" 5 10))
  |> GameState.addCustomAction
      (Action.init "hunt rats"
        |> Action.effect (AddToResource "rats" 2)
        |> Action.cooldown (60*Time.second))