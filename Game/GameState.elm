module Game.GameState exposing(..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Resource as Resource exposing (Resource)
import Game.Fire as Fire exposing (Fire)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  , fire : Fire
  }


updateGameTime : Time -> GameState -> GameState
updateGameTime t s = 
  { s | gameTime = s.gameTime + t }
  |> (updateResourceCooldowns t)
  |> (\s -> { s | fire = Fire.update t s.fire })


updateResourceCooldowns : Time -> GameState -> GameState
updateResourceCooldowns t s =
  { s | resources = List.map (Resource.updateCooldown t) s.resources }


harvestResource : Resource -> GameState -> GameState
harvestResource resource s = 
  { s | resources = List.map (\r -> if r.name == resource.name then
                                      Resource.harvest r
                                    else r) 
                             s.resources
  }


getResourceNamed : String -> GameState -> Maybe Resource
getResourceNamed name state =
  List.filter (\r -> r.name == name) state.resources
  |> List.head


mutateResource : String -> (Int -> Int) -> GameState -> GameState
mutateResource name fn s =
  { s | resources = List.map (\r -> if r.name == name then
                                      {r | amount = (fn r.amount)}
                                    else r) 
                             s.resources}


stokeFire : GameState -> GameState
stokeFire s =
  { s | fire = Fire.stoke s.fire }