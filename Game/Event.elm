module Game.Event exposing(..)

import Game.Story exposing(Choice, Mutator)


type Event = DisplayText String
             | DisplayChoices (List Choice)
             | TriggerMutator Mutator
             | TriggerStoryEvent String
