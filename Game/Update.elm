module Game.Update exposing (..)

import Time exposing(Time)

import Annex exposing (..)
import Queue.TimedQueue as TimedQueue

import Game.Constants as Constants
import Game.Model exposing(Model)
import Game.GameState as GameState exposing(GameState, Resource)
import Game.Story as Story exposing(StoryEvent, Choice, Consequence)
import Game.Event as Event exposing(Event)


type Message = -- Messages to control the running of the game
               TogglePause
               | Restart

               -- Messages sent in the normal course of play.
               | UpdateTime Time
               | MakeChoice Choice
               | CollectResource Resource


update : Message -> Model -> Model
update msg model =
  case msg of

    TogglePause -> togglePause model

    Restart -> restart model.paused

    UpdateTime time -> updateGame time model

    MakeChoice choice -> makeChoice choice model

    CollectResource resource -> collectResource resource model


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
  case m.activeChoices of

    Nothing ->
      updateGameTime m t
      |> triggerStoryEvents
      |> processEventQueue

    -- Game is soft-paused while we wait
    -- for the player to make a choice.
    other -> m


updateGameTime : Model -> Time -> Model
updateGameTime m t =
  { m | gameState = GameState.updateGameTime t m.gameState
      , eventQueue = TimedQueue.update t m.eventQueue 
  }


triggerStoryEvents : Model -> Model
triggerStoryEvents m =
  playStoryEvents (triggeredStoryEvents m.storyEventCorpus m.gameState) m
  |> removeStoryEvents


triggeredStoryEvents : List StoryEvent -> GameState -> List StoryEvent
triggeredStoryEvents events state =
  List.filter (\e -> e.trigger state) events


-- Remove triggered events that can only be triggered once.
removeStoryEvents : Model -> Model
removeStoryEvents m =
  { m | storyEventCorpus =
          List.filter (\e -> not ((e.trigger m.gameState) && e.occursOnce)) 
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


enqueueTextEvents : StoryEvent -> Model -> Model
enqueueTextEvents event m =
  case event.text of
    [] -> m
    first::rest ->
      let 
        m2 = enqueueTextEventWithDelay 
              first Constants.firstMessageDelay m
      in
        List.foldl enqueueTextEvent m2 rest


enqueueTextEvent : String -> Model -> Model
enqueueTextEvent text m =
  enqueueTextEventWithDelay text Constants.defaultMessageDelay m


enqueueTextEventWithDelay : String -> Time -> Model -> Model
enqueueTextEventWithDelay text delay m =
  let 
    textEvent = Event.DisplayText text
  in
    { m | eventQueue = TimedQueue.enqueue 
                          textEvent 
                          delay
                          m.eventQueue 
    }


enqueueChoiceEvent : StoryEvent -> Model -> Model
enqueueChoiceEvent event m =
  case event.choices of
    Nothing -> m
    Just choices ->
      let
        choiceEvent = Event.DisplayChoices choices
      in
        { m | eventQueue = TimedQueue.enqueue 
                              choiceEvent
                              Constants.choiceButtonsDelay
                              m.eventQueue
        }


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
      m


displayText : String -> Model -> Model
displayText text m =
  { m | messageHistory = text::m.messageHistory }


displayChoices : List Choice -> Model -> Model
displayChoices choices m =
  { m | activeChoices = Just choices }


makeChoice : Choice -> Model -> Model
makeChoice choice m =
  clearActiveChoices <|
  case choice.consequence of
    Nothing -> m
    Just c -> playConsequence c m


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
  

collectResource : Resource -> Model -> Model
collectResource r m = m
