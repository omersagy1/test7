module Parser.Build exposing(..)

import Game.Mutators as Mutators exposing (Mutator)
import Game.Triggers as Triggers exposing (Trigger)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)


-- Event with default values that can
-- be "filled in" with other values.
newEvent : StoryEvent
newEvent =
  { name = ""
  , trigger = Triggers.manualOnly
  , text = []
  , choices = Nothing
  , occursOnce = True
  , mutator = Nothing
  } 

name : String -> StoryEvent -> StoryEvent
name n e = { e | name = n }

trigger : Trigger -> StoryEvent -> StoryEvent
trigger t e = { e | trigger = t }

setText : List String -> StoryEvent -> StoryEvent
setText t e = { e | text = t }

ln : String -> StoryEvent -> StoryEvent
ln t e = { e | text = e.text ++ [t] }

choices : List Choice -> StoryEvent -> StoryEvent
choices c e = { e | choices = Just c }

choice : Choice -> StoryEvent -> StoryEvent
choice c e =
  case e.choices of
    Nothing -> choices [c] e
    Just cs -> choices (cs ++ [c]) e

reoccurring : StoryEvent -> StoryEvent
reoccurring e = { e | occursOnce = False }

mutator : Mutator -> StoryEvent -> StoryEvent
mutator m e = { e | mutator = Just m }


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
