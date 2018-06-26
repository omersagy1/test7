module Game.Fire exposing (..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)


type alias Fire = 
  { strength : Cooldown,
    stokeCooldown : Cooldown
  }


init : Time -> Time -> Fire
init burnTime stokeCooldown = 
  { strength = Cooldown.new burnTime 
  , stokeCooldown = Cooldown.new stokeCooldown
  }


update : Time -> Fire -> Fire
update t f =
  { f | strength = Cooldown.update t f.strength 
      , stokeCooldown = Cooldown.update t f.stokeCooldown
  }


canStoke : Fire -> Bool
canStoke f = not (Cooldown.isCoolingDown f.stokeCooldown)


stoke : Fire -> Fire
stoke f = 
  if not (canStoke f) then f
  else
    { strength = Cooldown.start f.strength 
    , stokeCooldown = Cooldown.start f.stokeCooldown
    }


strength : Fire -> Float
strength f = Cooldown.currentFraction f.strength


isExtinguished : Fire -> Bool
isExtinguished f = not (Cooldown.isCoolingDown f.strength)