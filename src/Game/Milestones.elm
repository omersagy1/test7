module Game.Milestones exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Common.Annex exposing (..)

type alias Milestones = Dict String Milestone

type alias Milestone =
  { name : String
  , timeReached : Time
  , counter : Int
  }


init : Milestones
init = Dict.empty


inc : Milestone -> Milestone
inc m = { m | counter = m.counter + 1 }


hasReached : String -> Milestones -> Bool
hasReached name milestones =
  Dict.member name milestones


setReached : String -> Time -> Milestones -> Milestones
setReached name currentTime milestones =
  Dict.insert name (newMilestone name currentTime |> inc) milestones


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
  Dict.update name (\maybem -> case maybem of 
                                Nothing ->
                                  newMilestone name time |> inc |> Just
                                Just m -> 
                                  Just (inc m))
              milestones

counter : String -> Milestones -> Int
counter name milestones =
  Dict.get name milestones 
  |> maybeChain .counter
  |> Maybe.withDefault 0