module Game.GameState exposing(..)

import Time exposing(Time)


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  , resources : List Resource
  }


type alias Resource =
  { name : String
  , amount : Int 
  }


updateGameTime : Time -> GameState -> GameState
updateGameTime t s = { s | gameTime = s.gameTime + t }