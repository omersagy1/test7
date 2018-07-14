module Parser.Build exposing (..)

import Game.Condition as Condition exposing (Condition)
import Game.Effect exposing (Effect)
import Game.StoryEvent exposing (..)

-- BUILDERS THAT BEGIN AN EVENT SEQUENCE

start : StoryEvent
start = Atomic Empty

narrate : String -> StoryEvent
narrate s = Atomic <| Narration s

-- CONVENIENCE BUILDERS FOR SEQUENCING 

seq : StoryEvent -> StoryEvent -> StoryEvent
seq next = (\prev -> Compound <| Sequenced prev next)

ln : String -> StoryEvent -> StoryEvent
ln s = seq (Atomic <| Narration s)

effect : Effect -> StoryEvent -> StoryEvent
effect eff = seq (Atomic <| Effectful eff)

cond : Condition -> StoryEvent -> StoryEvent -> StoryEvent
cond c e = seq (Compound <| Conditioned c e)

choices : List Choice -> StoryEvent -> StoryEvent
choices choices = seq (Compound <| PlayerChoice choices)

goto : String -> StoryEvent -> StoryEvent
goto ref = seq (Atomic <| Goto ref)

rand : List StoryEvent -> StoryEvent -> StoryEvent
rand opts = seq (Compound <| Random opts)

-- BUILDERS FOR CHOICES

choice : String -> Choice
choice prompt =
  { condition = Condition.Pure Condition.Always
  , prompt = prompt
  , consq = Atomic Empty
  }

consq : StoryEvent -> Choice -> Choice
consq e c = { c | consq = e }

condition : Condition -> Choice -> Choice
condition cond choice = { choice | condition = cond }

-- BUILDERS FOR TOP-LEVEL EVENTS

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