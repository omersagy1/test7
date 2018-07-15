module Common.Animation exposing (..)

import Time exposing (Time)


-- Animates linearly between 0 and 1.
type Animation = Linear LinearParams

type alias LinearParams =
  { timePassed : Time
  , totalDuration : Time
  }


init : Time -> Animation
init duration = Linear { timePassed = 0, totalDuration = duration }


update : Time -> Animation -> Animation
update timePassed anim =
  case anim of
    Linear params -> 
      Linear { params | 
                timePassed = min (params.timePassed + timePassed) params.totalDuration }


currentValue : Animation -> Float
currentValue anim =
  case anim of
    Linear params -> params.timePassed / params.totalDuration


complete : Animation -> Bool
complete anim = (currentValue anim) == 1