module Game.Action exposing (..)

import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Effect as Effect exposing (Effect)
import Game.Resource exposing (Resource)


type Action = StokeFire
              | HarvestResource Resource
              | CA CustomAction


type alias CustomAction =
  { name : String
  , active: Bool
  , effect : Effect
  , cooldown : Cooldown
  }


init : String -> CustomAction
init name =
  { name = name
  , active = False
  , effect = Effect.NoEffect
  , cooldown = Cooldown.new 0
  }


effect : Effect -> CustomAction -> CustomAction
effect e a = { a | effect = e }


activate : CustomAction -> CustomAction
activate a = { a | active = True }


deactivate : CustomAction -> CustomAction
deactivate a = { a | active = False }