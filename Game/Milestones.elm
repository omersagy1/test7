module Game.Milestones exposing (..)

import Dict
import Time exposing (Time)


type alias Milestones = Dict String Milestone


init : Milestones
init = Dict.empty


hasReached : String -> Milestones -> Bool
hasReached name milestones =
  Dict.member name milestones


setReached : String -> Time -> Milestones -> Milestones
setReached name currentTime milestones =
  Dict.insert name (newMilestone name currentTime) milestones


type alias Milestone =
  { name : String
  , timeReached : Time
  }


newMilestone : String -> Time -> Milestone
newMilestone name timeReached = 
  { name = name
  , timeReached = timeReached
  }
