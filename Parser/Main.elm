module Parser.Main exposing (..)

import Time

import Data.Config as Config
import Game.Story as Story exposing (
  StoryEvent, Choice, Consequence, Trigger, Mutator)


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ blankEvent
    |> setName "begin"
    |> setTrigger (Story.gameTimePassed (1 * Time.second))
    |> addTextLine "hello world!"
  ,
    blankEvent
    |> setName "mystery-man"
    |> setTrigger (Story.gameTimePassed (3 * Time.second))
    |> addTextLine "A mysterious squirrel has appeared."
    |> addTextLine "What do you want to do?"
    |> addChoice
        (blankChoice 
         |> setChoiceText "Wait"
         |> setConsequenceEvent 
             (blankEvent
              |> addTextLine "nothing happens..."))
    |> addChoice
        (blankChoice
         |> setChoiceText "Kill it"
         |> setConsequenceName "squirrel-killed")
  ,
    blankEvent
    |> setName "squirrel-killed"
    |> addTextLine "It's dead now."
  ]


-- Event with default values that can
-- be "filled in" with other values.
blankEvent : StoryEvent
blankEvent =
  { name = ""
  , trigger = Story.manualOnly
  , text = []
  , choices = Nothing
  , occursOnce = True
  , mutator = Nothing
  } 

setName : String -> StoryEvent -> StoryEvent
setName n e = { e | name = n }

setTrigger : Trigger -> StoryEvent -> StoryEvent
setTrigger t e = { e | trigger = t }

setText : List String -> StoryEvent -> StoryEvent
setText t e = { e | text = t }

addTextLine : String -> StoryEvent -> StoryEvent
addTextLine t e = { e | text = e.text ++ [t] }

setChoices : List Choice -> StoryEvent -> StoryEvent
setChoices c e = { e | choices = Just c }

addChoice : Choice -> StoryEvent -> StoryEvent
addChoice c e =
  case e.choices of
    Nothing -> setChoices [c] e
    Just choices -> setChoices (choices ++ [c]) e

setOccursOnce : Bool -> StoryEvent -> StoryEvent
setOccursOnce o e = { e | occursOnce = o }

setMutator : Mutator -> StoryEvent -> StoryEvent
setMutator m e = { e | mutator = Just m }


blankChoice : Choice
blankChoice =
  { text = ""
  , consequence = Nothing
  }

setChoiceText : String -> Choice -> Choice
setChoiceText t c = { c | text = t }

setConsequenceName : String -> Choice -> Choice
setConsequenceName n c = 
  { c | consequence = Just (Story.EventName n) }

setConsequenceEvent : StoryEvent -> Choice -> Choice
setConsequenceEvent e c =
  { c | consequence = Just (Story.ActualEvent e) }


parse : String -> List StoryEvent
parse config =
  String.trim config
  |> String.split "\n\n"
  |> List.map parseEvent


parseEvent : String -> StoryEvent
parseEvent s = blankEvent
