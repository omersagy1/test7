module Game.Story exposing(..)

import Game.GameState exposing(..)


type alias StoryEvent = 
  { trigger: Trigger 
  , text: List String 
  , choices: List Choice
  }


type alias Choice =
  { text : String
  , consequence : Maybe Event
  } 


type alias Trigger = GameState -> Bool
