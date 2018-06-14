module Game.Model exposing(..)

import Time exposing(Time)

import Queue.TimedQueue as TimedQueue

import Game.Story exposing(StoryEvent, Trigger)
import Game.GameState exposing(GameState)
import Game.Event exposing(Event)


initialModel : Model
initialModel = 
  { gameState = initialGameState 
  , messageHistory = []
  , eventQueue = TimedQueue.new
  , storyEventCorpus = Game.Story.storyEventCorpus
  , paused = False
  }


initialGameState : GameState
initialGameState =
  { gameTime = 0
  , resources = []
  }


type alias Model = 

  -- State of the game on a semantic level; i.e.
  -- gameState only contains things relevant to
  -- the conceptual understanding of the game, not
  -- the state of the machinery.
  { gameState : GameState

  -- Messages to be displayed on-screen.
  , messageHistory : List String 

  -- Events waiting to be executed.
  , eventQueue : TimedQueue.TimedQueue Event

  -- All story events that could be triggered.
  , storyEventCorpus : List StoryEvent

  -- Whether the game receives update time events.
  , paused : Bool
  }
