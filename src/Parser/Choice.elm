module Parser.Choice exposing (..)

import Game.Story as Story exposing (Choice, Consequence)

new : Choice
new =
  { text = ""
  , consequenceSet = []
  }

text : String -> Choice -> Choice
text t c = { c | text = t }

consq : Consequence -> Choice -> Choice
consq consq c = { c | consequenceSet = c.consequenceSet ++ [consq]}