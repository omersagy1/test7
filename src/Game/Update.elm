module Game.Update exposing (..)

import Time exposing (Time)

import Common.Annex exposing (..)
import Common.Randomizer as Randomizer exposing (Randomizer)
import Queue.TimedQueue as TimedQueue
import Game.Action as Action exposing (Action)
import Game.ConditionFns as ConditionFns
import Game.Constants as Constants
import Game.Effect as Effect exposing (Effect)
import Game.Event as Event exposing (Event)
import Game.GameState as GameState exposing (GameState)
import Game.Model exposing (Model)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence, EventOrName)


-- Messages to control the running of the game
type Message = TogglePause
               | ToggleFastForward
               | Restart
               | UpdateTime Time
               | MakeChoice Choice
               | GameplayMessage Action
               | StartTime Time


update : Message -> Model -> Model
update msg model =
  case msg of

    StartTime t -> initialize t model
    
    TogglePause -> togglePause model

    ToggleFastForward -> toggleFastForward model

    Restart -> restart model.paused

    MakeChoice choice -> 
      if hardPaused model then model
      else
        makeChoice choice model

    UpdateTime time -> 
      if gameplayPaused model then model
      else
        updateGame time model

    GameplayMessage action -> 
      if gameplayPaused model then model
      else
        { model | gameState = processUserAction action model.gameState }


initialize : Time -> Model -> Model
initialize t m =
  { m | gameState = GameState.initRandomizer t m.gameState }


gameplayPaused : Model -> Bool
gameplayPaused m =
  hardPaused m || waitingOnChoice m


hardPaused : Model -> Bool
hardPaused m = m.paused


waitingOnChoice : Model -> Bool
waitingOnChoice m = m.activeChoices /= Nothing


processUserAction : Action -> GameState -> GameState
processUserAction msg state =
  case msg of 

    Action.StokeFire ->
      GameState.stokeFire state
    
    Action.CA customAction ->
      GameState.performCustomAction customAction state


togglePause : Model -> Model
togglePause m = { m | paused = not m.paused }


toggleFastForward : Model -> Model
toggleFastForward m = { m | fastForward = not m.fastForward }


restart : Bool -> Model
restart paused = 
  let
    fresh = Game.Model.initialModel
  in
    { fresh | paused = paused }


updateGame : Time -> Model -> Model
updateGame t m =
  updateGameTime t m
  |> triggerStoryEvents
  |> processEventQueue
  |> clearActions 


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


clearActions : Model -> Model
clearActions m = { m | gameState = GameState.clearActions m.gameState }


triggerStoryEvents : Model -> Model
triggerStoryEvents m =
  let
    (triggeredEvents, newModel) =
      triggeredStoryEvents m.storyEventCorpus m
  in
    playStoryEvents triggeredEvents newModel


-- Returns a list of triggered story events, and the state
-- with the corpus updated.
triggeredStoryEvents : List StoryEvent -> Model -> (List StoryEvent, Model)
triggeredStoryEvents events model =
  let
    (triggered, remaining, state) = helper model.gameState events [] []
  in
    ( triggered
    , { model | gameState = state
              , storyEventCorpus = remaining 
      })


helper : GameState -> List StoryEvent -> List StoryEvent -> List StoryEvent -> (List StoryEvent, List StoryEvent, GameState)
helper state toscan triggered remaining =
  case toscan of
    [] -> (triggered, remaining, state)
    first::rest ->
      let
        (eventTriggered, newState) = (ConditionFns.condition(first.trigger) state)
        triggered2 = if eventTriggered then first::triggered else triggered
        shouldRemove = eventTriggered && first.occursOnce
        remaining2 = if shouldRemove then remaining else first::remaining
      in
        helper newState rest triggered2 remaining2


playStoryEvents : List StoryEvent -> Model -> Model
playStoryEvents events m =
  case events of
    [] -> m
    event::rest ->
      playStoryEvent event m |> (playStoryEvents rest)


playStoryEvent : StoryEvent -> Model -> Model
playStoryEvent event m = 
  enqueueTextEvents event m
  |> enqueueChoiceEvent event
  |> enqueueEffect event
  |> enqueueSubsequent event


enqueueEvent : Event -> Time -> Model -> Model
enqueueEvent event delay m =
    { m | eventQueue = TimedQueue.enqueue 
                          event 
                          delay
                          m.eventQueue 
    }


enqueueTextEvents : StoryEvent -> Model -> Model
enqueueTextEvents event m =
  case event.text of
    [] -> m
    first::rest ->
      let 
        firstMessageDelay = 
          if eventQueueEmpty m then Constants.firstMessageDelay
          else Constants.firstMessageNonEmptyQueueDelay
        (txt, m2) = getText first m
        m3 = enqueueEvent (Event.DisplayText txt) firstMessageDelay m2
        (textLines, m4) = 
          foldingMutate getText rest m3
      in
        List.foldl enqueueTextEvent m4 textLines


getText : Story.Line -> Model -> (String, Model)
getText ln m =
  let
    (txt, newRandomizer) = Story.getText ln m.gameState.randomizer
  in
    ( Maybe.withDefault "randomizer code is broken!" txt
    , updateRandomizer newRandomizer m
    )


updateRandomizer : Randomizer -> Model -> Model
updateRandomizer r m = 
  { m | gameState = GameState.updateRandomizer r m.gameState }


enqueueTextEvent : String -> Model -> Model
enqueueTextEvent text m =
  enqueueEvent
    (Event.DisplayText text) Constants.defaultMessageDelay m


eventQueueEmpty : Model -> Bool
eventQueueEmpty m = (TimedQueue.size m.eventQueue) == 0


enqueueChoiceEvent : StoryEvent -> Model -> Model
enqueueChoiceEvent event m =
  case event.choices of
    Nothing -> m
    Just choices ->
      enqueueEvent 
        (Event.DisplayChoices choices)
        Constants.choiceButtonsDelay
        m


enqueueEffect : StoryEvent -> Model -> Model
enqueueEffect event m =
  case event.effect of
    Nothing -> m
    Just mutator ->
      enqueueEvent 
        (Event.ApplyEffect mutator)
        Constants.mutatorDelay
        m


enqueueSubsequent : StoryEvent -> Model -> Model
enqueueSubsequent event m =
  let
    (maybeConsq, s) = Story.getConsequence event.subsequents m.gameState
    m2 = setGameState s m
  in
    case maybeConsq of
      Nothing -> m2
      Just consq ->
        enqueueEvent (Event.TriggerStoryEvent (Story.eventName consq))
                      Constants.triggerStoryEventDelay
                      m2


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
      displayText text m
    Event.DisplayChoices choices ->
      displayChoices choices m
    Event.TriggerStoryEvent name ->
      (maybePerform playStoryEvent) (getStoryEventByName name m) m
    Event.ApplyEffect e ->
      applyEffect e m


displayText : String -> Model -> Model
displayText text m =
  { m | messageHistory = text::m.messageHistory }


displayChoices : List Choice -> Model -> Model
displayChoices choices m =
  { m | activeChoices = Just choices }


applyEffect : Effect -> Model -> Model
applyEffect effect model =
  { model | gameState = GameState.applyEffect effect model.gameState }


makeChoice : Choice -> Model -> Model
makeChoice choice model =
  let
    (c, s) = Story.getConsequence choice.consequenceSet model.gameState
  in
    model
    |> setGameState s
    |> (maybePerform playEventOrName) c
    |> clearActiveChoices


setGameState : GameState -> Model -> Model
setGameState s m = { m | gameState = s }


clearActiveChoices : Model -> Model
clearActiveChoices m =
  { m | activeChoices = Nothing }


playEventOrName : EventOrName -> Model -> Model
playEventOrName e m =
  case e of

    Story.ActualEvent storyEvent -> 
      playStoryEvent storyEvent m

    Story.EventName name ->
      let 
        event = getStoryEventByName name m
      in 
        case event of
          Nothing -> m
          Just e -> playStoryEvent e m


getStoryEventByName : String -> Model -> Maybe StoryEvent
getStoryEventByName name model =
  let
    matches = List.filter (\e -> e.name == name) model.storyEventCorpus
  in
    case matches of
      [] -> Nothing
      [e] -> Just e
      -- Only return the first match.
      e::others -> Just e
