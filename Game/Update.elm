module Game.Update exposing (..)

import Time exposing(Time)

import Game.Model exposing(Model)
import Game.GameState as GameState exposing(GameState)
import Game.Story exposing(StoryEvent)


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


playStoryEvents : List StoryEvent -> Model -> Model
playStoryEvents events m =
  case events of
    [] -> m
    (event::rest) ->
      playStoryEvent event m |> (playStoryEvents rest)


playStoryEvent : StoryEvent -> Model -> Model
playStoryEvent event m = m


triggeredStoryEvents : List StoryEvent -> GameState -> List StoryEvent
triggeredStoryEvents events state =
  List.filter (\e -> e.trigger state) events


processEventQueue : Model -> Model
processEventQueue m = m