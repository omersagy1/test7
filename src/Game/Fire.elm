module Game.Fire exposing (..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)


type alias Fire = { strength : Cooldown }


init : Time -> Fire
init burnTime = { strength = Cooldown.new burnTime }


update : Time -> Fire -> Fire
update t f = { f | strength = Cooldown.update t f.strength }


stoke : Fire -> Fire
stoke f = { strength = Cooldown.start f.strength }


strength : Fire -> Float
strength f = Cooldown.currentFraction f.strength


isExtinguished : Fire -> Bool
isExtinguished f = not (Cooldown.isCoolingDown f.strength)