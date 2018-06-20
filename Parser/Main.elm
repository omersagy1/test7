module Parser.Main exposing (..)

import Time

import Data.Config as Config
import Game.Story as Story exposing (
  StoryEvent, Choice, Consequence, Trigger, Mutator)


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
    |> name "begin"
    |> trigger (Story.gameTimePassed (1 * Time.second))
    |> ln "hello world!"
  ,
    newEvent
    |> name "mystery-man"
    |> trigger (Story.gameTimePassed (3 * Time.second))
    |> ln "A mysterious squirrel has appeared."
    |> ln "What do you want to do?"
    |> choice
        (newChoice 
         |> text "Wait"
         |> consq 
             (newEvent
              |> ln "nothing happens..."))
    |> choice
        (newChoice
         |> text "Kill it"
         |> consqName "squirrel-killed")
  ,
    newEvent
    |> name "squirrel-killed"
    |> ln "It's dead now."
    |> ln "Weirdly, there was a bit of gold in its fur."
    |> mutator (Story.mutateResource "gold" ((+) 10))
  ,
    newEvent
    |> trigger (Story.resourceAbove "wood" 5)
    |> ln "Looks like you've got some wood."
  ]


-- Event with default values that can
-- be "filled in" with other values.
newEvent : StoryEvent
newEvent =
  { name = ""
  , trigger = Story.manualOnly
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

occursOnce : Bool -> StoryEvent -> StoryEvent
occursOnce o e = { e | occursOnce = o }

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


parse : String -> List StoryEvent
parse config =
  String.trim config
  |> String.split "\n\n"
  |> List.map parseEvent


parseEvent : String -> StoryEvent
parseEvent s = newEvent
