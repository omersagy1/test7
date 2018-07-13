module Parser.Build exposing (..)

import Game.Condition as Condition exposing (Condition)
import Game.Effect exposing (Effect)
import Game.StoryEvent exposing (..)

topLevel : TopLevelEvent
topLevel = TopLevel
  { name = ""
  , reoccurring = False
  , trigger = Condition.Pure Condition.Never
  , event = Atomic Empty
  }


name : String -> TopLevelEvent -> TopLevelEvent
name s (TopLevel p) = TopLevel { p | name = s }

reoccurring : TopLevelEvent -> TopLevelEvent
reoccurring (TopLevel p) = TopLevel { p | reoccurring = True }

trigger : Condition -> TopLevelEvent -> TopLevelEvent
trigger t (TopLevel p) = TopLevel { p | trigger = t }

body : StoryEvent -> TopLevelEvent -> TopLevelEvent
body e (TopLevel p) = TopLevel { p | event = e }

start : StoryEvent
start = Atomic Empty

seq : StoryEvent -> StoryEvent -> StoryEvent
seq next prev = Compound <| Sequenced prev next

ln : String -> StoryEvent -> StoryEvent
ln s e = seq (Atomic <| Narration s) e

narrate : String -> StoryEvent
narrate s = Atomic <| Narration s

effect : Effect -> StoryEvent -> StoryEvent
effect eff e = seq (Atomic <| Effectful eff) e 

cond : Condition -> StoryEvent -> StoryEvent -> StoryEvent
cond c e prev = seq (Compound <| Conditioned c e) prev