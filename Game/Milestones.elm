module Game.Milestones exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Annex exposing (..)

type alias Milestones = Dict String Milestone

type alias Milestone =
  { name : String
  , timeReached : Time
  }


init : Milestones
init = Dict.empty


hasReached : String -> Milestones -> Bool
hasReached name milestones =
  Dict.member name milestones


setReached : String -> Time -> Milestones -> Milestones
setReached name currentTime milestones =
  Dict.insert name (newMilestone name currentTime) milestones


newMilestone : String -> Time -> Milestone
newMilestone name timeReached = 
  { name = name
  , timeReached = timeReached
  }


timeSince : String -> Time -> Milestones -> Maybe Time
timeSince name currentTime milestones =
  Dict.get name milestones
  |> maybeChain .timeReached
  |> maybeChain (\t -> currentTime - t)