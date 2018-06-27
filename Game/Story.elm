module Game.Story exposing(..)

import Game.Effect as Effect exposing (Effect)
import Game.Triggers as Triggers exposing (Trigger)


type alias StoryEvent = 
  { name: String
  , trigger: Trigger 
  , text: List String 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  , effect : Maybe Effect
  }


type alias Choice =
  { text : String
  , consequence : Maybe Consequence
  } 


type Consequence = ActualEvent StoryEvent
                   | EventName String
