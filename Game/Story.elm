module Game.Story exposing(..)

import List
import Time exposing(Time)

import Game.GameState as GameState exposing(GameState)


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


-- TRIGGERS 

type alias Trigger = GameState -> Bool

gameTimePassed : Time -> Trigger
gameTimePassed t = (\s -> s.gameTime >= t)

manualOnly : Trigger
manualOnly s = False


-- MUTATORS

type alias Mutator = GameState -> GameState

mutateResource : String -> (Int -> Int) -> Mutator
mutateResource = GameState.mutateResource
