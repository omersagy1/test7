module Game.GameState exposing(..)

import Time exposing (Time)

import Annex exposing (..)
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
  }


init : GameState
init =
  { gameTime = 0
  , resources = []
  , fire = Fire.init 0 0
  , actionHistory = ActionHistory.newHistory
  , milestones = Milestones.init
  , customActions = []
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
  |> updateActionCooldowns t
  |> (\s -> { s | fire = Fire.update t s.fire })


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
    |> applyEffect a.effect
    |> addAction (Action.CA a)


type alias Mutator = GameState -> GameState

applyEffect : Effect -> GameState -> GameState
applyEffect e s =
  case e of
    Effect.NoEffect -> s
    Effect.ActivateResource name -> (activateResource name) s
    Effect.AddToResource name x -> (addToResource name x) s
    Effect.SubtractResource name x -> (subtractResource name x) s
    Effect.SetResourceAmount name x -> (setResourceAmount name x) s
    Effect.SetMilestoneReached name -> setMilestoneReached name s
    Effect.IncrementMilestone name -> incrementMilestone name s
    Effect.ActivateAction name -> (activateAction name) s
    Effect.DeactivateAction name -> (deactivateAction name) s
    Effect.Compound effects -> List.foldl applyEffect s effects
    Effect.Compound2 e1 e2 -> List.foldl applyEffect s [e1, e2]


activateResource : String -> Mutator
activateResource name =
  (\s -> applyToResource name (Resource.activate) s)


addToResource : String -> Int -> Mutator
addToResource name x = 
  (\s ->
    let stateWithActiveResource =
      if not (resourceActive name s) then
        applyToResource name (Resource.activate) s
      else
        s
    in
      applyToResource 
        name (Resource.add x) stateWithActiveResource)


subtractResource : String -> Int -> Mutator
subtractResource name x = 
  applyToResource name (Resource.subtract x)


setResourceAmount : String -> Int -> Mutator
setResourceAmount name x = 
  applyToResource name (Resource.mutate (\_ -> x))


and : Mutator -> Mutator -> Mutator
and m1 m2 = (\s -> s |> m1 |> m2)


activateAction : String -> Mutator
activateAction name =
  (\s -> applyToAction name (Action.activate) s)


deactivateAction : String -> Mutator
deactivateAction name =
  (\s -> applyToAction name (Action.deactivate) s)