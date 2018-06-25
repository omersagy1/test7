module Game.GameState exposing(..)

import Time exposing (Time)

import Annex exposing (..)
import Game.Cooldown as Cooldown exposing (Cooldown)
import Game.Resource as Resource exposing (Resource)
import Game.Fire as Fire exposing (Fire)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  , fire : Fire
  }


init : GameState
init =
  { gameTime = 0
  , resources = []
  , fire = Fire.init 0
  }


addResource : Resource -> GameState -> GameState
addResource r s =
  { s | resources = s.resources ++ [r] }


setFireCooldown : Time -> GameState -> GameState
setFireCooldown t s =
  { s | fire = Fire.init t }


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


stokeFire : GameState -> GameState
stokeFire s =
  if (resourceAmount "wood" s) <= 0 then s
  else
    { s | fire = Fire.stoke s.fire }
    |> applyToResource "wood" (Resource.subtract 1)

    