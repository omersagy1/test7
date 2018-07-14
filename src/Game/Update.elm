module Game.Update exposing (..)

import Time exposing (Time)
import Task

import Common.Annex exposing (..)
import Queue.TimedQueue as TimedQueue
import Game.Action as Action exposing (Action)
import Game.Constants as Constants
import Game.Condition as Condition exposing (Condition)
import Game.ConditionFns as ConditionFns
import Game.Effect as Effect exposing (Effect)
import Game.Event as Event exposing (Event)
import Game.GameState as GameState exposing (GameState)
import Game.Model as Model exposing (Model)
import Game.Story as Story
import Game.StoryEvent as StoryEvent exposing (..)


-- Messages to control the running of the game
type Message = TogglePause
               | ToggleFastForward
               | Restart
               | UpdateTime Time
               | MakeChoice Choice
               | GameplayMessage Action
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
      if Model.gameplayPaused model then model
      else
        updateGame time model

    GameplayMessage action -> 
      if Model.gameplayPaused model then model
      else
        { model | gameState = processUserAction action model.gameState }


processUserAction : Action -> GameState -> GameState
processUserAction msg state =
  case msg of 

    Action.StokeFire ->
      GameState.stokeFire state
    
    Action.CA customAction ->
      GameState.performCustomAction customAction state


updateGame : Time -> Model -> Model
updateGame t m =
  updateGameTime t m
  |> triggerStoryEvents
  |> processEventQueue
  |> Model.clearActions 


updateGameTime : Time -> Model -> Model
updateGameTime timePassed m =
    let
      t = if m.fastForward 
          then timePassed * Constants.fastForwardFactor        
          else timePassed
    in
      { m | gameState = GameState.update t m.gameState
          , eventQueue = TimedQueue.update t m.eventQueue 
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
    |> playStoryEvents triggeredEvents


playStoryEvents : List StoryEvent -> Model -> Model
playStoryEvents events model =
  model |>
  case events of
    [] -> identity
    event::rest ->
      playStoryEvent event >> playStoryEvents rest


playStoryEvent : StoryEvent -> Model -> Model
playStoryEvent event model = 
  model |>
  case event of
    StoryEvent.Atomic e -> 
      playAtomicEvent e
    StoryEvent.Compound e -> 
      playCompoundEvent e
    StoryEvent.Sequenced events ->
      playSequencedEvent events


playAtomicEvent : AtomicEvent -> Model -> Model
playAtomicEvent e model =
  model |>
  case e of
    Narration ln -> enqueueTextEvent ln
    Effectful eff -> enqueueEffect eff
    Goto ref -> enqueueGotoEvent ref
    other -> identity


playCompoundEvent : CompoundEvent -> Model -> Model
playCompoundEvent e model =
  model |>
  case e of
    Conditioned c e -> 
      playConditionedEvent c e
    PlayerChoice choices -> 
      playChoice choices
    Random options ->
      playRandomEvent options
    Ranked options ->
      playRankedEvent options


playSequencedEvent : List StoryEvent -> Model -> Model
playSequencedEvent events model =
  model |>
  case events of
    [] -> identity
    first::rest -> playStoryEvent first >> playSequencedEvent rest


playConditionedEvent : Condition -> StoryEvent -> Model -> Model
playConditionedEvent c e m =
  let
      (success, state) = ConditionFns.condition c m.gameState
  in
    Model.setGameState state m |>
    if success then
      (playStoryEvent e)
    else identity


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
    |> enqueueChoiceEvent choicesToDisplay


playRandomEvent : List StoryEvent -> Model -> Model
playRandomEvent events m =
  Model.choose events m
  |> (\(event, model) -> (maybePerform playStoryEvent event model))


playRankedEvent : List StoryEvent -> Model -> Model
playRankedEvent events m = m


enqueueEvent : Event -> Time -> Model -> Model
enqueueEvent event delay m =
    { m | eventQueue = TimedQueue.enqueue 
                          event 
                          delay
                          m.eventQueue 
    }


enqueueTextEvent : String -> Model -> Model
enqueueTextEvent text m =
  enqueueEvent
    (Event.DisplayText text) Constants.defaultMessageDelay m


eventQueueEmpty : Model -> Bool
eventQueueEmpty m = (TimedQueue.size m.eventQueue) == 0


enqueueChoiceEvent : List Choice -> Model -> Model
enqueueChoiceEvent choices m =
  enqueueEvent (Event.DisplayChoices choices) Constants.choiceButtonsDelay m


enqueueGotoEvent : String -> Model -> Model
enqueueGotoEvent name m =
  enqueueEvent (Event.TriggerStoryEvent name) 0 m


enqueueEffect : Effect -> Model -> Model
enqueueEffect eff m =
  enqueueEvent (Event.ApplyEffect eff) Constants.mutatorDelay m


dequeueEvent : Model -> (Maybe Event, Model)
dequeueEvent m =
  let 
    (e, queue) = (TimedQueue.dequeue m.eventQueue)
  in
    (e, { m | eventQueue = queue })


processEventQueue : Model -> Model
processEventQueue m = 
  let 
    (e, newModel) = dequeueEvent m
  in
    case e of
      Nothing -> newModel
      Just event -> processEvent event newModel


processEvent : Event -> Model -> Model
processEvent e m =
  case e of
    Event.DisplayText text ->
      Model.displayText text m
    Event.DisplayChoices choices ->
      Model.displayChoices choices m
    Event.TriggerStoryEvent name ->
      (maybePerform playStoryEvent) 
        (Story.getEventByName name m.story |> maybeChain StoryEvent.getEvent) m
    Event.ApplyEffect e ->
      Model.applyEffect e m


makeChoice : Choice -> Model -> Model
makeChoice choice model =
  Model.clearActiveChoices model
  |> playStoryEvent choice.consq