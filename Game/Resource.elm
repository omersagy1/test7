module Game.Resource exposing (..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)


type alias Resource =
  { name : String
  , amount : Int 
  , harvestIncrement : Int
  , cooldown : Cooldown
  }


init : String -> Resource
init name =
  { name = name
  , amount = 0 
  , harvestIncrement = 0
  , cooldown = Cooldown.new 0
  }


initialAmount : Int -> Resource -> Resource
initialAmount x r = { r | amount = x }


harvestIncrement : Int -> Resource -> Resource
harvestIncrement x r = { r | harvestIncrement = x }


cooldown : Time -> Resource -> Resource
cooldown t r = { r | cooldown = Cooldown.new t }


updateCooldown : Time -> Resource -> Resource
updateCooldown t r =
  { r | cooldown = Cooldown.update t r.cooldown }


harvest : Resource -> Resource
harvest r =
  if not (Cooldown.isCoolingDown r.cooldown) then
    { r | amount = r.amount + r.harvestIncrement
        , cooldown = Cooldown.start r.cooldown
    }
  else
    r


mutate : (Int -> Int) -> Resource -> Resource
mutate fn r =
  { r | amount = fn r.amount }
