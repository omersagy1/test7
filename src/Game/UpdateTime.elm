module Game.UpdateTime exposing (..)

import Time exposing (Time)

import Common.Annex exposing (..)
import Queue.TimedQueue as TimedQueue
import Game.Constants as Constants
import Game.Condition as Condition exposing (Condition)
import Game.ConditionFns as ConditionFns
import Game.GameState as GameState exposing (GameState)
import Game.Model as Model exposing (Model)
import Game.Printer as Printer
import Game.Story as Story
import Game.StoryEvent as StoryEvent exposing (..)


updateGame : Time -> Model -> Model
updateGame t m =
  let
    timePassed = 
        if m.fastForward 
        then t * Constants.fastForwardFactor        
        else t
  in
    m
    |> updateGameTime timePassed
    |> Printer.update timePassed
    |> triggerStoryEvents
    |> (if not (Printer.isPrinting m) then processEventQueue else identity)
    |> Model.clearActionHistory 


updateGameTime : Time -> Model -> Model
updateGameTime timePassed m =
  let
    newState = if m.interactionMode then m.gameState
               else GameState.update timePassed m.gameState
  in
    { m | gameState = newState
        , eventQueue = TimedQueue.update timePassed m.eventQueue 
    }


triggerStoryEvents : Model -> Model
triggerStoryEvents m =
  let
    (triggeredEvents, story, gameState) =
      Story.triggeredStoryEvents m.gameState m.story
  in
    { m | gameState = gameState
        , story = story
    }
    |> enqueueStoryEvents triggeredEvents


enqueueStoryEvents : List StoryEvent -> Model -> Model
enqueueStoryEvents events model =
  model |>
  case events of
    [] -> identity
    event::rest -> enqueueStoryEvent event


enqueueStoryEvent : StoryEvent -> Model -> Model
enqueueStoryEvent event m =
  let
    delay = if Model.eventQueueEmpty m then
              Constants.firstMessageDelay
            else 
              Constants.firstMessageNonEmptyQueueDelay
  in
    enqueueStoryEventWithDelay event delay m


enqueueStoryEventWithDelay : StoryEvent -> Time -> Model -> Model
enqueueStoryEventWithDelay event delay m =
      { m | eventQueue = TimedQueue.enqueue 
                            event 
                            delay
                            m.eventQueue 
      }


pushStoryEvent : StoryEvent -> Model -> Model
pushStoryEvent event m =
  pushStoryEventWithDelay event (getDelay event) m


pushStoryEventWithDelay : StoryEvent -> Time -> Model -> Model
pushStoryEventWithDelay event delay m =
      { m | eventQueue = TimedQueue.push 
                            event 
                            delay
                            m.eventQueue 
      }


getDelay : StoryEvent -> Time
getDelay event =
  case event of
    Atomic (Narration _) -> Constants.defaultMessageDelay
    Atomic (Dialogue _) -> Constants.defaultMessageDelay
    Atomic (Effectful _) -> Constants.mutatorDelay
    Atomic (Goto _) -> 0
    PlayerChoice _ -> Constants.choiceButtonsDelay
    other -> 0


processEventQueue : Model -> Model
processEventQueue m = 
  let 
    (e, newModel) = dequeueEvent m
  in
    case e of
      Nothing -> newModel
      Just event -> playStoryEvent event newModel


dequeueEvent : Model -> (Maybe StoryEvent, Model)
dequeueEvent m =
  let 
    (e, queue) = (TimedQueue.dequeue m.eventQueue)
  in
    (e, { m | eventQueue = queue })


playStoryEvent : StoryEvent -> Model -> Model
playStoryEvent event model = 
  model |>
  case event of
    Atomic e -> 
      playAtomicEvent e
    Sequenced events -> 
      playSequencedEvent events
    Conditioned (ConditionedEvent c e) -> 
      playConditionedEvent c e
    PlayerChoice choices -> 
      playChoice choices
    Random options ->
      playRandomEvent options
    Cases cases ->
      playCasesEvent cases


playAtomicEvent : AtomicEvent -> Model -> Model
playAtomicEvent e model =
  model |>
  case e of
    Narration ln -> 
      displayText ln
    
    Dialogue ln ->
      displayDialogue ln

    Effectful eff ->
      Model.applyEffect eff

    Goto ref ->
      (maybePerform playStoryEvent) 
        (Story.getEventByName ref model.story 
         |> Maybe.map StoryEvent.getEvent)
    
    StartInteraction ->
      (\model -> { model | interactionMode = True })

    EndInteraction ->
      (\model -> { model | interactionMode = False })


playSequencedEvent : List StoryEvent -> Model -> Model
playSequencedEvent events model =
  model |>
  case events of
    [] -> identity
    first::rest ->
      pushStoryEvents (List.reverse rest)
      >> pushStoryEventWithDelay first Constants.postChoiceMessageDelay


pushStoryEvents : List StoryEvent -> Model -> Model
pushStoryEvents events model =
  model |>
  case events of
    [] -> identity
    first::rest -> pushStoryEvent first >> pushStoryEvents rest


playConditionedEvent : Condition -> StoryEvent -> Model -> Model
playConditionedEvent c e m =
  let
      (success, state) = ConditionFns.condition c m.gameState
  in
    Model.setGameState state m |>
    if success then
      (pushStoryEvent e)
    else identity


playRandomEvent : List StoryEvent -> Model -> Model
playRandomEvent events m =
  Model.choose events m
  |> (\(event, model) -> (maybePerform pushStoryEvent event model))


playCasesEvent : List ConditionedEvent -> Model -> Model
playCasesEvent events m =
  case events of
    [] -> m
    (ConditionedEvent condition event)::rest ->
      let 
        (success, state) = ConditionFns.condition condition m.gameState
      in
        Model.setGameState state m |>
        if success then
          pushStoryEvent event
        else
          playCasesEvent rest


playChoice : List Choice -> Model -> Model
playChoice choices m =
  let
    (choicesToDisplay, newState) = 
      filterMutate (\choice gameState -> 
                      ConditionFns.condition choice.condition gameState) 
                   choices 
                   m.gameState
  in
    Model.setGameState newState m
    |> Model.displayChoices choicesToDisplay


displayText : String -> Model -> Model
displayText text model =
  Printer.setActiveMessage text model


displayDialogue : String -> Model -> Model
displayDialogue text model =
  displayText ("\"" ++ text ++ "\"") model