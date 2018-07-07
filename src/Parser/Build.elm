module Parser.Build exposing (..)

import Game.Effect as Effect exposing (Effect)
import Game.Condition as Condition exposing (Condition, PureCondition, RandomCondition)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)
import Parser.Consequence as Consequence
import Parser.Choice as Choice


-- Event with default values that can
-- be "filled in" with other values.
newEvent : StoryEvent
newEvent =
  { name = ""
  , trigger = (Condition.P Condition.Never)
  , text = []
  , choices = Nothing
  , occursOnce = True
  , effect = Nothing
  , subsequents = []
  } 

name : String -> StoryEvent -> StoryEvent
name n e = { e | name = n }

trigger : Condition -> StoryEvent -> StoryEvent
trigger c e = { e | trigger = c }

setText : List String -> StoryEvent -> StoryEvent
setText t e = { e | text = List.map Story.FixedLine t }

ln : String -> StoryEvent -> StoryEvent
ln t e = { e | text = e.text ++ [Story.FixedLine t] }

randlns : List String -> StoryEvent -> StoryEvent
randlns lns e = { e | text = e.text ++ [Story.RandomLines lns] }

choices : List Choice -> StoryEvent -> StoryEvent
choices c e = { e | choices = Just c }

choice : Choice -> StoryEvent -> StoryEvent
choice c e =
  case e.choices of
    Nothing -> choices [c] e
    Just cs -> choices (cs ++ [c]) e

reoccurring : StoryEvent -> StoryEvent
reoccurring e = { e | occursOnce = False }

effect : Effect -> StoryEvent -> StoryEvent
effect effect event = { event | effect = Just effect }

goto : String -> StoryEvent -> StoryEvent
goto name e = 
  { e | subsequents = [ Consequence.new |> Consequence.ref name ] }

subsq : Consequence -> StoryEvent -> StoryEvent
subsq c e = { e | subsequents = e.subsequents ++ [c] }


-- redefined functions --

newChoice = Choice.new
text = Choice.text
consq = Choice.consq

newConsq = Consequence.new
ref = Consequence.ref
event = Consequence.event
cond = Consequence.cond
