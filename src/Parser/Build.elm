module Parser.Build exposing(..)

import Game.Effect as Effect exposing (Effect)
import Game.Condition as Condition exposing (Condition, PureCondition)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)


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
  , goto = Nothing
  } 

name : String -> StoryEvent -> StoryEvent
name n e = { e | name = n }

trigger : PureCondition -> StoryEvent -> StoryEvent
trigger c e = { e | trigger = Condition.P c }

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
goto name e = { e | goto = Just (Story.EventName name) }


newChoice : Choice
newChoice =
  { text = ""
  , consequence = Nothing
  }

text : String -> Choice -> Choice
text t c = { c | text = t }

consqName : String -> Choice -> Choice
consqName n c = 
  { c | consequence = Just (Story.EventName n) }

consq : StoryEvent -> Choice -> Choice
consq e c = { c | consequence = Just (Story.ActualEvent e) }
