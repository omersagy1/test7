module Game.Update exposing (..)

import Time exposing(Time)

import Game.Model exposing(Model)
import Game.GameState as GameState exposing(GameState)


type Message = UpdateTime Time


update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateTime time -> updateGame model time


updateGame : Model -> Time -> Model
updateGame m t =
  updateGameTime m t
  |> triggerStoryEvents
  |> processEventQueue


updateGameTime : Model -> Time -> Model
updateGameTime m t =
  { m | gameState = GameState.updateGameTime m.gameState t }


triggerStoryEvents : Model -> Model
triggerStoryEvents m = m


processEventQueue : Model -> Model
processEventQueue m = m