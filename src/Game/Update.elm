module Game.Update exposing (..)

import Time exposing (Time)
import Task

import Common.Annex exposing (..)
import Queue.TimedQueue as TimedQueue
import Game.Action as Action exposing (Action)
import Game.Constants as Constants
import Game.Condition as Condition exposing (Condition)
import Game.ConditionFns as ConditionFns
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
    Conditioned c e -> 
      playConditionedEvent c e
    PlayerChoice choices -> 
      playChoice choices
    Random options ->
      playRandomEvent options
    Ranked options ->
      playRankedEvent options


playAtomicEvent : AtomicEvent -> Model -> Model
playAtomicEvent e model =
  model |>
  case e of
    Narration ln -> 
      Model.displayText ln

    Effectful eff ->
      Model.applyEffect eff

    Goto ref ->
      (maybePerform playStoryEvent) 
        (Story.getEventByName ref model.story |> maybeChain StoryEvent.getEvent)

    other -> identity


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


playRankedEvent : List StoryEvent -> Model -> Model
playRankedEvent events m = m


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