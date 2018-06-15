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
mutateResource name fn =
  GameState.updateResource name fn


-- ACTUAL STORY EVENTS

storyEventCorpus = 
  [ { name = "begin"
    , trigger = gameTimePassed (1 * Time.second)
    , text = [ "hello world!" ]
    , choices = Nothing 
    , occursOnce = True
    , mutator = Nothing
    }
  ,
    { name = "mystery-man"
    , trigger = gameTimePassed (3 * Time.second)
    , text = 
      [ "A mysterious squirrel has appeared."
      , "What do you want to do?"
      ]
    , choices = Just
      [ { text = "Wait"
        , consequence = Nothing
        }
      , { text = "Kill it"
        , consequence = Just (EventName "squirrel-killed")
        }
      ]
    , occursOnce = True
    , mutator = Nothing
    }
  ,
    { name = "squirrel-killed"
    , trigger = manualOnly
    , text = ["It's dead now."]
    , choices = Nothing
    , occursOnce = True
    , mutator = Nothing
    }
  ]
