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


updateGameTime : GameState -> Time -> GameState
updateGameTime s t = { s | gameTime = s.gameTime + t }