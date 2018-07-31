module Parser.Build exposing (..)

import Game.ActionName as ActionName
import Game.Condition as Condition exposing (Condition)
import Game.Effect exposing (Effect)
import Game.StoryEvent exposing (..)
import Parser.Condition exposing (..)

-- BUILDERS THAT BEGIN AN EVENT SEQUENCE

start : StoryEvent
start = Sequenced []

narrate : String -> StoryEvent
narrate s = start |> ln s

speak : String -> StoryEvent
speak s = start |> di s

-- CONVENIENCE BUILDERS FOR SEQUENCING 

seq : StoryEvent -> StoryEvent -> StoryEvent
seq next = (\prev -> append prev next)

ln : String -> StoryEvent -> StoryEvent
ln s = seq (Atomic <| Narration s)

di : String -> StoryEvent -> StoryEvent
di s = seq (Atomic <| Dialogue s)

effect : Effect -> StoryEvent -> StoryEvent
effect eff = seq (Atomic <| Effectful eff)

cond : Condition -> StoryEvent -> StoryEvent -> StoryEvent
cond c e = seq (Conditioned <| ConditionedEvent c e)

choices : List Choice -> StoryEvent -> StoryEvent
choices choices = seq (PlayerChoice choices)

goto : String -> StoryEvent -> StoryEvent
goto ref = seq (Atomic <| Goto ref)

rand : List StoryEvent -> StoryEvent -> StoryEvent
rand opts = seq (Random opts)

caseif : Condition -> StoryEvent -> List ConditionedEvent -> List ConditionedEvent
caseif c e list = list ++ [ConditionedEvent c e]

cases : List ConditionedEvent -> StoryEvent -> StoryEvent
cases events = seq (Cases events)

default : StoryEvent -> List ConditionedEvent -> List ConditionedEvent
default e list = caseif unconditionally e list

caseList : List ConditionedEvent
caseList = []

restrict : StoryEvent -> StoryEvent
restrict = seq (Atomic StartInteraction)

resume : StoryEvent -> StoryEvent
resume = seq (Atomic EndInteraction)


-- BUILDERS FOR CHOICES

choice : String -> Choice
choice prompt =
  { condition = Condition.Pure Condition.Always
  , prompt = prompt
  , consq = Sequenced []
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
  , event = Sequenced []
  }

name : String -> TopLevelEvent -> TopLevelEvent
name s (TopLevel p) = TopLevel { p | name = s }

reoccurring : TopLevelEvent -> TopLevelEvent
reoccurring (TopLevel p) = TopLevel { p | reoccurring = True }

trigger : Condition -> TopLevelEvent -> TopLevelEvent
trigger t (TopLevel p) = TopLevel { p | trigger = t }

body : StoryEvent -> TopLevelEvent -> TopLevelEvent
body e (TopLevel p) = TopLevel { p | event = e }


-- EFFECT BUILDERS

ud : String -> ActionName.Name
ud x = ActionName.UserDefined x