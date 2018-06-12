module Game.Model exposing(..)

import Time exposing(Time)


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
  , eventQueue : List Event

  -- All story events that could be triggered.
  , storyEventCorpus : List StoryEvent

  }


type alias GameState =
  -- Time passed in the game so far.
  { gameTime : Time
  }


type Event = DisplayText String
             | DisplayChoice List String 


type alias Choice =
  { text : String
  , consequence : Maybe Event
  } 


type alias StoryEvent = 
  { text: List String 
  , trigger: Trigger 
  }


type alias Trigger = GameState -> Bool


