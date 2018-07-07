module Game.GameState exposing(..)

import Time exposing (Time)

import Common.Annex exposing (..)
import Common.Randomizer as Randomizer exposing (Randomizer)
import Game.Action as Action exposing (Action, CustomAction)
import Game.ActionHistory as ActionHistory exposing (ActionHistory)
import Game.Effect as Effect exposing (Effect)
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
  , customActions : List CustomAction
  , randomizer : Randomizer
  }


init : GameState
init =
  { gameTime = 0
  , resources = []
  , fire = Fire.init 0 0
  , actionHistory = ActionHistory.newHistory
  , milestones = Milestones.init
  , customActions = []
  , randomizer = Randomizer.init 0
  }


update : Time -> GameState -> GameState
update t s = 
  { s | gameTime = s.gameTime + t }
  |> updateActionCooldowns t
  |> (\s -> { s | fire = Fire.update t s.fire })


initRandomizer : Time -> GameState -> GameState
initRandomizer t = updateRandomizer (Randomizer.init t)


updateRandomizer : Randomizer -> GameState -> GameState
updateRandomizer randomizer s = { s | randomizer = randomizer}


addResource : Resource -> GameState -> GameState
addResource r s =
  { s | resources = s.resources ++ [r] }


initFire : Time -> Time -> GameState -> GameState
initFire burnTime stokeCooldown s =
  { s | fire = Fire.init burnTime stokeCooldown }


updateActionCooldowns : Time -> GameState -> GameState
updateActionCooldowns t s =
  { s | customActions = List.map (Action.updateCooldown t) s.customActions }


activeResources : GameState -> List Resource
activeResources s =
  List.filter (\r -> r.active) s.resources

activeActions : GameState -> List CustomAction
activeActions s =
  List.filter (\a -> a.active) s.customActions

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
            ActionHistory.addAction a s.actionHistory}


actionPerformed : Action -> GameState -> Bool
actionPerformed a s = ActionHistory.hasAction a s.actionHistory


customActionPerformed : String -> GameState -> Bool
customActionPerformed name s = 
  ActionHistory.hasCustomAction name s.actionHistory


clearActions : GameState -> GameState
clearActions s = 
  { s | actionHistory = ActionHistory.clearActions s.actionHistory }


milestoneReached : String -> GameState -> Bool
milestoneReached name s = Milestones.hasReached name s.milestones


setMilestoneReached : String -> GameState -> GameState
setMilestoneReached name s =
  { s | milestones = Milestones.setReached name s.gameTime s.milestones }


incrementMilestone : String -> GameState -> GameState
incrementMilestone name s =
  { s | milestones = Milestones.increment name s.gameTime s.milestones }


milestoneCounter : String -> GameState -> Int
milestoneCounter name s = Milestones.counter name s.milestones


timeSince : String -> GameState -> Maybe Time
timeSince name s = Milestones.timeSince name s.gameTime s.milestones


addCustomAction : CustomAction -> GameState -> GameState
addCustomAction a s = { s | customActions = a::s.customActions }


applyToAction : String -> (CustomAction -> CustomAction) -> GameState -> GameState
applyToAction name fn s =
  { s | customActions = List.map (\a -> if a.name == name then
                                          fn a 
                                        else a) 
                             s.customActions
  }


performCustomAction : CustomAction -> GameState -> GameState
performCustomAction a s =
  if not (Action.canPerform a) then s
  else
    applyToAction a.name Action.performAction s
    |> (applyEffect a.effect) 
    |> addAction (Action.CA a)


applyEffect : Effect -> GameState -> GameState
applyEffect e s =
  case e of
    Effect.NoEffect -> s 

    Effect.ActivateResource name -> 
      applyToResource name (Resource.activate) s

    Effect.AddToResource name x -> 
      addToResource name x s

    Effect.AddToResourceRand name x y -> 
      addToResourceRand name x y s 

    Effect.SubtractResource name x ->
      applyToResource name (Resource.subtract x) s

    Effect.SetResourceAmount name x ->
      applyToResource name (Resource.mutate (\_ -> x)) s

    Effect.SetMilestoneReached name -> 
      setMilestoneReached name s

    Effect.IncrementMilestone name -> 
      incrementMilestone name s

    Effect.ActivateAction name -> 
      applyToAction name (Action.activate) s

    Effect.DeactivateAction name -> 
      applyToAction name (Action.deactivate) s

    Effect.Compound effects -> 
      List.foldl applyEffect s effects

    Effect.Compound2 e1 e2 -> 
      List.foldl applyEffect s [e1, e2]


addToResource : String -> Int -> GameState -> GameState
addToResource name x s = 
  let stateWithActiveResource =
    if not (resourceActive name s) then
      applyToResource name (Resource.activate) s
    else
      s
  in
    applyToResource 
      name (Resource.add x) stateWithActiveResource


addToResourceRand : String -> Int -> Int -> GameState -> GameState
addToResourceRand name x y s =
  let
    (val, r) = Randomizer.int x y s.randomizer
  in
    addToResource name val s
    |> (\s -> { s | randomizer = r })