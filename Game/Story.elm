module Game.Story exposing(..)

import List
import Time exposing (Time)

import Game.GameState as GameState exposing (GameState)
import Game.Mutators as Mutators exposing (Mutator)
import Game.Triggers as Triggers exposing (Trigger)


type alias StoryEvent = 
  { name: String
  , trigger: Trigger 
  , text: List String 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  , mutator : Maybe Mutator
  }


type alias Choice =
  { text : String
  , consequence : Maybe Consequence
  } 


type Consequence = ActualEvent StoryEvent
                   | EventName String
