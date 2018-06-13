module Game.Story exposing(..)

import Game.GameState exposing(..)
import Game.Event exposing(Event)


type alias StoryEvent = 
  { name: String
  , trigger: Trigger 
  , text: List String 
  , choices: List Choice
  }


type alias Choice =
  { text : String
  , consequence : Maybe Event
  } 


type alias Trigger = GameState -> Bool
