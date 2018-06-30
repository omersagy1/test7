module Game.Update exposing (..)

import Time exposing (Time)

import Annex exposing (..)
import Randomizer
import Queue.TimedQueue as TimedQueue
import Game.Action as Action exposing (Action)
import Game.Condition as Condition
import Game.Constants as Constants
import Game.Effect as Effect exposing (Effect)
import Game.Event as Event exposing (Event)
import Game.GameState as GameState exposing (GameState)
import Game.Model exposing (Model)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)


-- Messages to control the running of the game
type Message = TogglePause
               | Restart
               | UpdateTime Time
               | MakeChoice Choice
               | GameplayMessage Action


update : Message -> Model -> Model
update msg model =
  case msg of

    TogglePause -> togglePause model

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


restart : Bool -> Model
restart paused = 
  let
    fresh = Game.Model.initialModel
  in
    { fresh | paused = paused }


updateGame : Time -> Model -> Model
updateGame t m =
  initRandomizer t m
  |> updateGameTime t
  |> triggerStoryEvents
  |> processEventQueue
  |> clearActions 


initRandomizer : Time -> Model -> Model
initRandomizer t m =
  case m.randomizer of
    Nothing -> { m | randomizer = Just (Randomizer.init t) }
    other -> m


updateGameTime : Time -> Model -> Model
updateGameTime t m =
  let
    timePassed : Time
    timePassed = case m.lastFrameTime of
                   Nothing -> 0
                   Just lastFrame -> t - lastFrame
  in
    { m | gameState = GameState.updateGameTime timePassed m.gameState
        , eventQueue = TimedQueue.update timePassed m.eventQueue 
        , lastFrameTime = Just t
    }


clearActions : Model -> Model
clearActions m = { m | gameState = GameState.clearActions m.gameState }


triggerStoryEvents : Model -> Model
triggerStoryEvents m =
  playStoryEvents (triggeredStoryEvents m.storyEventCorpus m.gameState) m
  |> removeStoryEvents


triggeredStoryEvents : List StoryEvent -> GameState -> List StoryEvent
triggeredStoryEvents events state =
  List.filter (\e -> Condition.conditionFn(e.trigger) state) events


-- Remove Conditioned events that can only be Conditioned once.
removeStoryEvents : Model -> Model
removeStoryEvents m =
  { m | storyEventCorpus =
          List.filter (\e -> not ((Condition.conditionFn(e.trigger) 
                                    m.gameState) 
                                  && e.occursOnce)) 
                      m.storyEventCorpus
  }


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
  |> enqueueMutator event
  |> enqueueGoto event


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
        m2 = enqueueEvent
              (Event.DisplayText first) firstMessageDelay m
      in
        List.foldl enqueueTextEvent m2 rest


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


enqueueMutator : StoryEvent -> Model -> Model
enqueueMutator event m =
  case event.effect of
    Nothing -> m
    Just mutator ->
      enqueueEvent 
        (Event.ApplyEffect mutator)
        Constants.mutatorDelay
        m


enqueueGoto : StoryEvent -> Model -> Model
enqueueGoto event m =
  case event.goto of
    Nothing -> m
    Just goto ->
      let 
        name = case goto of
                  Story.EventName n -> n
                  Story.ActualEvent e -> e.name
      in
        enqueueEvent 
          (Event.TriggerStoryEvent name)
          Constants.triggerStoryEventDelay
          m


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
makeChoice choice m =
  clearActiveChoices <|
  (maybePerform playConsequence) choice.consequence m


clearActiveChoices : Model -> Model
clearActiveChoices m =
  { m | activeChoices = Nothing }


playConsequence : Consequence -> Model -> Model
playConsequence consequence m =
  case consequence of

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
