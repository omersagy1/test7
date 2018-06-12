module Game.Model exposing(..)

import Time exposing(Time)


initialModel : Model
initialModel = 
  { gameTime = 0 
  , messageHistory = []
  , eventQueue = []
  , storyEventCorpus = []
  }


type alias Model = 

  -- Time passed in the game so far.
  { gameTime : Time

  -- Messages to be displayed on-screen.
  , messageHistory : List String 

  -- Events waiting to be executed.
  , eventQueue : List Event

  -- All story events that could be triggered.
  , storyEventCorpus : List Event

  }

type Event = DisplayText String
             | DisplayChoice List String 

type alias Choice =
  { text : String
  , consequence : Maybe Event
  } 

type alias StoryEvent = { text: List String }

-- MESSAGES --

type Message = DummyMessage


