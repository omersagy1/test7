module Game.Update exposing (..)

import Time exposing (Time)
import Task

import Common.Annex exposing (..)
import Queue.TimedQueue as TimedQueue
import Game.Action as Action exposing (Action)
import Game.ActionName as ActionName
import Game.ActionSet as ActionSet
import Game.Constants as Constants
import Game.Condition as Condition exposing (Condition)
import Game.ConditionFns as ConditionFns
import Game.GameState as GameState exposing (GameState)
import Game.Model as Model exposing (Model)
import Game.Printer as Printer
import Game.Story as Story
import Game.StoryEvent as StoryEvent exposing (..)


-- Messages to control the running of the game
type Message = TogglePause
               | ToggleFastForward
               | Restart
               | UpdateTime Time
               | MakeChoice Choice
               | GameplayMessage ActionName.Name
               | StartTime Time


command : Message -> Cmd Message
command msg =
  case msg of
    Restart -> Task.perform StartTime Time.now
    other -> Cmd.none


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
        updateGame time model

    GameplayMessage action -> 
      if Model.gameplayPaused model then model
      else
        { model | gameState = performAction action model.gameState }


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
    |> Model.clearActions 


updateGameTime : Time -> Model -> Model
updateGameTime timePassed m =
  let
    newState = if m.interactionMode then m.gameState
               else GameState.update timePassed m.gameState
  in
    { m | gameState = newState
        , eventQueue = TimedQueue.update timePassed m.eventQueue 
    }


performAction : ActionName.Name -> GameState -> GameState
performAction n s =
  let
    ma = ActionSet.getAction n s.actions
  in
    case ma of
      Nothing -> s
      Just a ->
        let
          (success, s2) = ConditionFns.condition a.condition s
        in
          if not ((Action.ready a) && success) then s2
          else
            GameState.applyToAction a.name Action.performAction s2
            |> GameState.applyEffect a.effect
            |> GameState.addAction a


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
        (Story.getEventByName ref model.story |> maybeChain StoryEvent.getEvent)
    
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


makeChoice : Choice -> Model -> Model
makeChoice choice model =
  Model.clearActiveChoices model
  |> pushStoryEventWithDelay choice.consq 0


displayText : String -> Model -> Model
displayText text model =
  Printer.setActiveMessage text model


displayDialogue : String -> Model -> Model
displayDialogue text model =
  displayText ("\"" ++ text ++ "\"") model