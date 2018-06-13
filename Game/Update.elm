module Game.Update exposing (..)

import Time exposing(Time)

import Game.Model exposing(Model)
import Game.GameState exposing(GameState)


type Message = UpdateTime Time


update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateTime t -> 
      { model | gameState = updateGameTime model.gameState t }


updateGameTime : GameState -> Time -> GameState
updateGameTime s t =
  { s | gameTime = s.gameTime + t }

