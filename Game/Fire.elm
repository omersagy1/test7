module Game.Fire exposing (..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)


type alias Fire = 
  { cooldown : Cooldown
  }


init : Time -> Fire
init burnTime = { cooldown = Cooldown.new burnTime }


update : Time -> Fire -> Fire
update t f =
  { f | cooldown = Cooldown.update t f.cooldown }


stoke : Fire -> Fire
stoke f = { cooldown = Cooldown.start f.cooldown }


strength : Fire -> Float
strength f = Cooldown.currentFraction f.cooldown


isExtinguished : Fire -> Bool
isExtinguished f = not (Cooldown.isCoolingDown f.cooldown)