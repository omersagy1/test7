module Game.Action exposing (..)

import Time exposing (Time)

import Game.ActionName exposing (..)
import Game.Condition as Condition exposing (Condition)
import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Effect as Effect exposing (Effect)


type alias Action =
  { name : Name
  , active: Bool
  , effect : Effect
  , cooldown : Cooldown
  , condition : Condition
  }


init : String -> Action
init name =
  { name = UserDefined name
  , active = False
  , effect = Effect.NoEffect
  , cooldown = Cooldown.new 0
  , condition = (Condition.Pure Condition.Always)
  }


fireAction : Time -> Action
fireAction cooldownTime =
  { name = StokeFire
  , active = True
  , effect = Effect.Compound2 Effect.StokeFire 
                              (Effect.SubtractResource "wood" 1)
  , cooldown = Cooldown.new cooldownTime
  , condition = (Condition.Pure (Condition.ResourceAmountAbove "wood" 1))
  }


effect : Effect -> Action -> Action
effect e a = { a | effect = e }


cooldown : Time -> Action -> Action
cooldown t a = { a | cooldown = Cooldown.new t }


updateCooldown : Time -> Action -> Action
updateCooldown t a =
  if not a.active then a
  else
    { a | cooldown = Cooldown.update t a.cooldown }


activate : Action -> Action
activate a = { a | active = True }


deactivate : Action -> Action
deactivate a = { a | active = False }


ready : Action -> Bool
ready a = not (Cooldown.isCoolingDown a.cooldown)


performAction : Action -> Action
performAction a =
  if not (ready a) then a
  else
    { a | cooldown = Cooldown.start a.cooldown }


nameAsString : Name -> String
nameAsString name = 
  case name of
    UserDefined n -> n
    StokeFire -> "stoke fire"