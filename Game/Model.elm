module Game.Model exposing(..)

import Time exposing(Time)

import Queue.TimedQueue as TimedQueue

import Game.Event exposing (Event)
import Game.GameState exposing (GameState)
import Game.Story exposing (StoryEvent, Choice)

import Parser.Main


initialModel : Model
initialModel = 
  { gameState = Parser.Main.initialGameState 
  , messageHistory = []
  , eventQueue = TimedQueue.new
  , storyEventCorpus = Parser.Main.storyEventCorpus
  , paused = False
  , activeChoices = Nothing
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

  -- The choice the player must make to continue the game.
  , activeChoices : Maybe (List Choice)
  
  }
