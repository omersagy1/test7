module Game.Model exposing(..)

import Time exposing(Time)

import Queue.TimedQueue exposing(TimedQueue)
import Game.Story exposing(StoryEvent, Trigger)
import Game.GameState exposing(GameState)

-- TODO --
-- update based on time
-- create story events and triggers
-- draw messages on screen
-- create multi-line events
-- add choices as issuable events
-- add resources and cooldown bars

initialModel : Model
initialModel = 
  { gameState = initialGameState 
  , messageHistory = []
  , eventQueue = []
  , storyEventCorpus = []
  }


initialGameState : GameState
initialGameState =
  { gameTime = 0
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
  , eventQueue : TimedQueue Event

  -- All story events that could be triggered.
  , storyEventCorpus : List StoryEvent

  }
