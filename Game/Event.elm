module Game.Event exposing(..)

import Game.Story exposing(Choice)


type Event = DisplayText String
             | DisplayChoices (List Choice)
             | TriggerStoryEvent String
