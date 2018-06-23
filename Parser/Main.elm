module Parser.Main exposing (..)

import Time

import Data.Config as Config
import Game.Cooldown as Cooldown
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource exposing (Resource)
import Game.Story as Story exposing (
  StoryEvent, Choice, Consequence, Trigger, Mutator)

import Parser.Build exposing(..)


initialGameState : GameState
initialGameState =
  
  GameState.init 

  |> GameState.addResource
     (Resource.init "gold" 
      |> Resource.initialAmount 0 
      |> Resource.harvestIncrement 5 
      |> Resource.cooldown (45*Time.second))

  |> GameState.addResource
     (Resource.init "wood"
      |> Resource.initialAmount 5 
      |> Resource.harvestIncrement 20 
      |> Resource.cooldown (25*Time.second))

  |> GameState.setFireCooldown (15*Time.second)


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
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
    |> mutator (Story.addToResource "gold" 10)
  ,
    newEvent
    |> trigger (Story.resourceAbove "wood" 10)
    |> ln "Looks like you've got some more wood."
    |> ln "Use it to keep the fire going."
  ]
