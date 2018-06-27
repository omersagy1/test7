module Game.Story exposing(..)

import Game.Effect as Effect exposing (Effect)
import Game.Condition as Condition exposing (Condition)


type alias StoryEvent = 
  { name: String
  , trigger: Condition 
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
