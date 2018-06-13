module Game.Model exposing(..)

import Time exposing(Time)

import Queue.TimedQueue as TimedQueue

import Game.Story exposing(StoryEvent, Trigger)
import Game.GameState exposing(GameState)
import Game.Event exposing(Event)

-- TODO --
-- create story events and triggers
-- draw messages on screen
-- create multi-line events
-- add choices as issuable events
-- add resources and cooldown bars

initialModel : Model
initialModel = 
  { gameState = initialGameState 
  , messageHistory = []
  , eventQueue = TimedQueue.new
  , storyEventCorpus = []
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

  }
