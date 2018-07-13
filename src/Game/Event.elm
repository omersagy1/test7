module Game.Event exposing(..)

import Game.Effect exposing (Effect)
import Game.StoryEvent exposing (Choice)


type Event = DisplayText String
             | DisplayChoices (List Choice)
             | ApplyEffect Effect
             | TriggerStoryEvent String
