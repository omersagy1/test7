module Game.Milestones exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Annex exposing (..)

type alias Milestones = Dict String Milestone

type alias Milestone =
  { name : String
  , timeReached : Time
  , counter : Int
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
  , counter = 0
  }


timeSince : String -> Time -> Milestones -> Maybe Time
timeSince name currentTime milestones =
  Dict.get name milestones
  |> maybeChain .timeReached
  |> maybeChain (\t -> currentTime - t)


increment : String -> Time -> Milestones -> Milestones
increment name time milestones = 
  Dict.update name (\m -> case m of 
                            Nothing ->
                              newMilestone name time
                              |> (\m -> { m | counter = 1 })
                              |> Just
                            Just ms -> 
                              Just { ms | counter = ms.counter + 1}) 
              milestones

counter : String -> Milestones -> Maybe Int
counter name milestones =
  Dict.get name milestones 
  |> maybeChain .counter