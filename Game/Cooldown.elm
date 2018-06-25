module Game.Cooldown exposing (..)

import Time exposing (Time)


type alias Cooldown =
  { timeRemaining : Time
  , duration : Time
  }


new : Time -> Cooldown
new duration =
  { timeRemaining = 0
  , duration = duration
  }


update : Time -> Cooldown -> Cooldown
update timePassed c = 
  { c | timeRemaining = max 0 c.timeRemaining - timePassed }


isCoolingDown : Cooldown -> Bool
isCoolingDown c = c.timeRemaining > 0


start : Cooldown -> Cooldown
start c = { c | timeRemaining = c.duration }


currentFraction : Cooldown -> Float
currentFraction c = c.timeRemaining / c.duration