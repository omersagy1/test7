module Game.Event exposing(..)

import Game.Mutators exposing (Mutator)
import Game.Story exposing (Choice)


type Event = DisplayText String
             | DisplayChoices (List Choice)
             | TriggerMutator Mutator
             | TriggerStoryEvent String
