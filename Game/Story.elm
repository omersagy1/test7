module Game.Story exposing(..)

import List
import Time exposing(Time)

import Game.GameState exposing(..)
import Game.Event exposing(..)


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

manualOnly : Trigger
manualOnly s = False


-- ACTUAL STORY EVENTS

storyEventCorpus = 
  [ { name = "begin"
    , trigger = gameTimePassed (1 * Time.second)
    , text = [ "hello world!" ]
    , choices = Nothing 
    , occursOnce = True
    }
  ,
    { name = "mystery-man"
    , trigger = gameTimePassed (3 * Time.second)
    , text = 
      [ "A mysterious man has appeared."
      , "What do you want to do?"
      ]
    , choices = Just
      [ { text = "Wait"
        , consequence = Nothing
        }
      , { text = "Kill him"
        , consequence = Just (TriggerStoryEvent "man-killed")
        }
      ]
    , occursOnce = True
    }
  ,
    { name = "man-killed"
    , trigger = manualOnly
    , text = ["He's dead now."]
    , choices = Nothing
    , occursOnce = True
    }
  ]
