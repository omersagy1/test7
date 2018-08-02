module Game.Update exposing (..)

import Time exposing (Time)
import Task

import Game.Action as Action exposing (Action)
import Game.ActionName as ActionName
import Game.ActionSet as ActionSet
import Game.ConditionFns as ConditionFns
import Game.GameState as GameState exposing (GameState)
import Game.Message exposing (..)
import Game.Model as Model exposing (Model)
import Game.StoryEvent as StoryEvent exposing (..)
import Game.UpdateTime as UpdateTime


update : Message -> Model -> Model
update msg model =
  case msg of

    StartTime t -> Model.initialize t model
    
    TogglePause -> Model.togglePause model

    ToggleFastForward -> Model.toggleFastForward model

    Restart -> Model.restart model.paused

    MakeChoice choice -> 
      if Model.hardPaused model then model
      else
        makeChoice choice model

    UpdateTime time -> 
      if Model.storyPaused model then model
      else
        UpdateTime.updateGame time model

    GameplayMessage action -> 
      if Model.gameplayPaused model then model
      else
        { model | gameState = performAction action model.gameState }


command : Message -> Cmd Message
command msg =
  case msg of
    Restart -> Task.perform StartTime Time.now
    other -> Cmd.none


makeChoice : Choice -> Model -> Model
makeChoice choice model =
  Model.clearActiveChoices model
  |> UpdateTime.pushStoryEventWithDelay choice.consq 0


performAction : ActionName.Name -> GameState -> GameState
performAction n s =
  if not (canPerformAction n s) then s
  else
    let
      ma = ActionSet.getAction n s.actions
    in
      case ma of
        Nothing -> s
        Just a ->
          GameState.applyToAction a.name Action.performAction s
          |> GameState.applyEffect a.effect
          |> GameState.addActionToHistory a


canPerformAction : ActionName.Name -> GameState -> Bool
canPerformAction n s =
  let
    action = ActionSet.getAction n s.actions
  in
    case action of
      Nothing -> False
      Just a ->
        (Action.ready a) && (ConditionFns.pure a.condition s)