module Game.Update exposing (..)

import Time exposing(Time)

import Queue.TimedQueue as TimedQueue

import Game.Model exposing(Model)
import Game.GameState as GameState exposing(GameState)
import Game.Story exposing(StoryEvent)
import Game.Event as Event exposing(Event)


type Message = UpdateTime Time
               | TogglePause 


update : Message -> Model -> Model
update msg model =
  case msg of
    UpdateTime time -> updateGame model time
    TogglePause -> togglePause model

togglePause : Model -> Model
togglePause m = { m | paused = not m.paused }

updateGame : Model -> Time -> Model
updateGame m t =
  updateGameTime m t
  |> triggerStoryEvents
  |> processEventQueue


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
    (event::rest) ->
      playStoryEvent event m |> (playStoryEvents rest)


playStoryEvent : StoryEvent -> Model -> Model
playStoryEvent event m = 
  enqueueTextEvents event m
  |> enqueueChoiceEvent event


enqueueTextEvents : StoryEvent -> Model -> Model
enqueueTextEvents event m =
  List.foldl enqueueTextEvent m event.text 


enqueueTextEvent : String -> Model -> Model
enqueueTextEvent text m =
  let 
    textEvent = Event.DisplayText text
  in
    { m | eventQueue = TimedQueue.enqueue 
                          textEvent 
                          (1*Time.second) 
                          m.eventQueue 
    }


enqueueChoiceEvent : StoryEvent -> Model -> Model
enqueueChoiceEvent event m = m


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
      (Just event) -> processEvent event newModel


processEvent : Event -> Model -> Model
processEvent e m =
  case e of
    Event.DisplayText text ->
      renderText text m
    Event.DisplayChoice choices ->
      m
    Event.TriggerStoryEvent name ->
      m


renderText : String -> Model -> Model
renderText text m =
  { m | messageHistory = text::m.messageHistory }
