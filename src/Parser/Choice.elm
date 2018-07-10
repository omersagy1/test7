module Parser.Choice exposing (..)

import Game.Story as Story exposing (Choice, Consequence, StoryEvent)
import Parser.Consequence as Consq

new : Choice
new =
  { text = ""
  , consequenceSet = []
  }

text : String -> Choice -> Choice
text t c = { c | text = t }

consq : Consequence -> Choice -> Choice
consq consq c = { c | consequenceSet = c.consequenceSet ++ [consq]}

directConsq : StoryEvent -> Choice -> Choice
directConsq event choice = { choice | consequenceSet = [Consq.new |> Consq.event event] }