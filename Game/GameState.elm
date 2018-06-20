module Game.GameState exposing(..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Fire as Fire exposing (Fire)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  , fire : Fire
  }


type alias Resource =
  { name : String
  , amount : Int 
  , harvestIncrement : Int
  , cooldown : Cooldown
  }


updateGameTime : Time -> GameState -> GameState
updateGameTime t s = 
  { s | gameTime = s.gameTime + t }
  |> (updateResourceCooldowns t)
  |> (\s -> { s | fire = Fire.update t s.fire })


updateResourceCooldowns : Time -> GameState -> GameState
updateResourceCooldowns t s =
  { s | resources = List.map (updateResourceCooldown t) s.resources }


updateResourceCooldown : Time -> Resource -> Resource
updateResourceCooldown t r =
  { r | cooldown = Cooldown.update t r.cooldown }


mutateResource : String -> (Int -> Int) -> GameState -> GameState
mutateResource name fn s =
  { s | resources = List.map (\r -> if r.name == name then
                                      {r | amount = (fn r.amount)}
                                    else r) 
                             s.resources}


harvestSingleResource : Resource -> GameState -> GameState
harvestSingleResource resource s = 
  { s | resources = List.map (\r -> if r.name == resource.name then
                                      harvestResource r
                                    else r) 
                             s.resources
  }


harvestResource : Resource -> Resource
harvestResource r =
  if not (Cooldown.isCoolingDown r.cooldown) then
    { r | amount = r.amount + r.harvestIncrement
        , cooldown = Cooldown.start r.cooldown
    }
  else
    r


getResourceNamed : String -> GameState -> Maybe Resource
getResourceNamed name state =
  List.filter (\r -> r.name == name) state.resources
  |> List.head


stokeFire : GameState -> GameState
stokeFire s =
  { s | fire = Fire.stoke s.fire }