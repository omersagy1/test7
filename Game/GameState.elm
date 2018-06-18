module Game.GameState exposing(..)

import Time exposing (Time)

import Game.Cooldown as Cooldown exposing (Cooldown)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  }


type alias Resource =
  { name : String
  , amount : Int 
  , cooldown : Cooldown
  }


updateGameTime : Time -> GameState -> GameState
updateGameTime t s = 
  { s | gameTime = s.gameTime + t }
  |> (updateResourceCooldowns t)


updateResourceCooldowns : Time -> GameState -> GameState
updateResourceCooldowns t s =
  { s | resources = List.map (updateCooldown t) s.resources }


updateCooldown : Time -> Resource -> Resource
updateCooldown t r =
  { r | cooldown = Cooldown.update t r.cooldown }


mutateResource : String -> (Int -> Int) -> GameState -> GameState
mutateResource name fn s =
  { s | resources = List.map (\r -> if r.name == name then
                                      {r | amount = (fn r.amount)}
                                    else r) 
                             s.resources}
