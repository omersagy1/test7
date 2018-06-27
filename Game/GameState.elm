module Game.GameState exposing(..)

import Time exposing (Time)

import Annex exposing (..)
import Game.Action as Action exposing (Action)
import Game.ActionHistory as ActionHistory exposing (ActionHistory)
import Game.Milestones as Milestones exposing (Milestones)
import Game.Resource as Resource exposing (Resource)
import Game.Fire as Fire exposing (Fire)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  , fire : Fire
  -- List of actions performed since the last update.
  -- Read by Condition to decide whether to Condition a
  -- StoryEvent.
  , actionHistory : ActionHistory
  , milestones : Milestones
  }


init : GameState
init =
  { gameTime = 0
  , resources = []
  , fire = Fire.init 0 0
  , actionHistory = ActionHistory.newHistory
  , milestones = Milestones.init
  }


addResource : Resource -> GameState -> GameState
addResource r s =
  { s | resources = s.resources ++ [r] }


initFire : Time -> Time -> GameState -> GameState
initFire burnTime stokeCooldown s =
  { s | fire = Fire.init burnTime stokeCooldown }


updateGameTime : Time -> GameState -> GameState
updateGameTime t s = 
  { s | gameTime = s.gameTime + t }
  |> (updateResourceCooldowns t)
  |> (\s -> { s | fire = Fire.update t s.fire })


updateResourceCooldowns : Time -> GameState -> GameState
updateResourceCooldowns t s =
  { s | resources = List.map (Resource.updateCooldown t) s.resources }


activeResources : GameState -> List Resource
activeResources s =
  List.filter (\r -> r.active) s.resources


resourceActive : String -> GameState -> Bool
resourceActive name s =
  List.member name (List.map .name (activeResources s))


getResourceNamed : String -> GameState -> Maybe Resource
getResourceNamed name state =
  List.filter (\r -> r.name == name) state.resources
  |> List.head


applyToResource : String -> (Resource -> Resource) -> GameState -> GameState
applyToResource name fn s =
  { s | resources = List.map (\r -> if r.name == name then
                                      fn r
                                    else r) 
                             s.resources}


resourceAmount : String -> GameState -> Int
resourceAmount name s =
  getResourceNamed name s
  |> maybeChain .amount
  |> Maybe.withDefault 0


canStokeFire : GameState -> Bool
canStokeFire s =
  (Fire.canStoke s.fire) && (resourceAmount "wood" s > 0)


stokeFire : GameState -> GameState
stokeFire s =
  if not (canStokeFire s) then s
  else
    { s | fire = Fire.stoke s.fire }
    |> applyToResource "wood" (Resource.subtract 1)
    |> addAction Action.StokeFire


addAction : Action -> GameState -> GameState
addAction a s = 
    { s | actionHistory = 
            ActionHistory.addAction Action.StokeFire s.actionHistory}


actionPerformed : Action -> GameState -> Bool
actionPerformed a s = ActionHistory.hasAction a s.actionHistory


clearActions : GameState -> GameState
clearActions s = 
  { s | actionHistory = ActionHistory.clearActions s.actionHistory }


milestoneReached : String -> GameState -> Bool
milestoneReached name s = Milestones.hasReached name s.milestones


setMilestoneReached : String -> GameState -> GameState
setMilestoneReached name s =
  { s | milestones = Milestones.setReached name s.gameTime s.milestones }


timeSince : String -> GameState -> Maybe Time
timeSince name s = Milestones.timeSince name s.gameTime s.milestones