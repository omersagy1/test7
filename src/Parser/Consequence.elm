module Parser.Consequence exposing (..)

import Game.Condition exposing (Condition)
import Game.Story as Story exposing (Consequence, StoryEvent)


new : Consequence
new =
  { eventOrName = Story.EventName ""
  , condition = Nothing
  }

name : String -> Consequence -> Consequence
name name c = { c | eventOrName = Story.EventName name }

event : StoryEvent -> Consequence -> Consequence
event e c = { c | eventOrName = Story.ActualEvent e }

cond : Condition -> Consequence -> Consequence
cond condition consequence =
  { consequence | condition = Just condition }
