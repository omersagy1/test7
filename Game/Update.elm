module Game.Update exposing (..)

import Time exposing(Time)

import Queue.TimedQueue as TimedQueue

import Game.Model exposing(Model)
import Game.GameState as GameState exposing(GameState)
import Game.Story exposing(StoryEvent)
import Game.Event as Event


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
triggerStoryEvents m =
  playStoryEvents
    (triggeredStoryEvents m.storyEventCorpus m.gameState)
    m


triggeredStoryEvents : List StoryEvent -> GameState -> List StoryEvent
triggeredStoryEvents events state =
  List.filter (\e -> e.trigger state) events


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


processEventQueue : Model -> Model
processEventQueue m = m