module Game.Action exposing (..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Effect as Effect exposing (Effect)


type Action = StokeFire
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


cooldown : Time -> CustomAction -> CustomAction
cooldown t a = { a | cooldown = Cooldown.new t }


updateCooldown : Time -> CustomAction -> CustomAction
updateCooldown t a =
  if not a.active then a
  else
    { a | cooldown = Cooldown.update t a.cooldown }


activate : CustomAction -> CustomAction
activate a = { a | active = True }


deactivate : CustomAction -> CustomAction
deactivate a = { a | active = False }


canPerform : CustomAction -> Bool
canPerform a = not (Cooldown.isCoolingDown a.cooldown)


performAction : CustomAction -> CustomAction
performAction a =
  if not (canPerform a) then a
  else
    { a | cooldown = Cooldown.start a.cooldown }