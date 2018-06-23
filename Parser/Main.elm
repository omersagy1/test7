module Parser.Main exposing (..)

import Time

import Data.Config as Config
import Game.Cooldown as Cooldown
import Game.GameState as GameState exposing (GameState)
import Game.Mutators as Mutators exposing (Mutator)
import Game.Resource as Resource exposing (Resource)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)
import Game.Triggers as Triggers exposing (Trigger)

import Parser.Build exposing (..)


initialGameState : GameState
initialGameState =
  
  GameState.init 

  |> GameState.setFireCooldown (15*Time.second)

  |> GameState.addResource
     (Resource.init "wood"
      |> Resource.initialAmount 5 
      |> Resource.harvestIncrement 20 
      |> Resource.cooldown (25*Time.second)
      |> Resource.activate)

  |> GameState.addResource
     (Resource.init "gold" 
      |> Resource.initialAmount 0 
      |> Resource.harvestIncrement 5 
      |> Resource.cooldown (45*Time.second))


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
    |> trigger (Triggers.gameTimePassed (3 * Time.second))
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
    |> ln "It's dead."
    |> ln "Weirdly, there was a bit of gold in its fur."
    |> ln "I guess you'll be hunting squirrels now."
    |> mutator (Mutators.addToResource "gold" 10)
  ,
    newEvent
    |> trigger (Triggers.resourceAbove "wood" 10)
    |> ln "Looks like you've got some more wood."
    |> ln "Use it to keep the fire going."
  ]
