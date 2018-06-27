module Game.Event exposing(..)

import Game.Effect exposing (Effect)
import Game.Story exposing (Choice)


type Event = DisplayText String
             | DisplayChoices (List Choice)
             | ApplyEffect Effect
             | ConditionStoryEvent String
