module Game.Story exposing(..)

import Common.Randomizer as Randomizer exposing (Randomizer)
import Game.Effect as Effect exposing (Effect)
import Game.Condition as Condition exposing (Condition)


type alias StoryEvent = 
  { name: String
  , trigger: Condition 
  , text: List Line 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  , effect : Maybe Effect
  , goto : Maybe Consequence
  }


type alias Choice =
  { text : String
  , consequence : Maybe Consequence
  } 


type Consequence = ActualEvent StoryEvent
                   | EventName String


type Line = FixedLine String
            | RandomLines (List String)


getText : Line -> Randomizer -> (Maybe String, Randomizer)
getText ln randomizer =
  case ln of
    FixedLine str -> (Just str, randomizer)
    RandomLines strs -> Randomizer.choose strs randomizer