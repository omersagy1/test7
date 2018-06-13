module Game.Story exposing(..)

import List
import Time exposing(Time)

import Game.GameState exposing(..)
import Game.Event exposing(Event)


type alias StoryEvent = 
  { name: String
  , trigger: Trigger 
  , text: List String 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  }


type alias Choice =
  { text : String
  , consequence : Maybe Event
  } 


triggeredStoryEvents : List StoryEvent -> GameState -> List StoryEvent
triggeredStoryEvents events state =
  List.filter (\e -> e.trigger state) events


-- TRIGGERS 

type alias Trigger = GameState -> Bool

gameTimePassed : Time -> Trigger
gameTimePassed t = (\s -> s.gameTime >= t)


-- ACTUAL STORY EVENTS

storyEventCorpus = 
  [
    { name = "begin"
    , trigger = gameTimePassed (1 * Time.second)
    , text = [ "hello world!" ]
    , choices = Nothing 
    , occursOnce = True
    }
  ]
