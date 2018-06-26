module Parser.Main exposing (..)

import Time

import Game.GameState as GameState exposing (GameState)
import Game.Mutators as Mutators exposing (Mutator)
import Game.Resource as Resource exposing (Resource)
import Game.Story as Story exposing (StoryEvent, Choice, Consequence)
import Game.Triggers as Triggers exposing (Trigger)

import Parser.Build exposing (..)


initialGameState : GameState
initialGameState =
  
  GameState.init 

  |> GameState.initFire (30*Time.second) (10*Time.second)

  |> GameState.addResource
     (Resource.init "wood"
      |> Resource.initialAmount 0 
      |> Resource.harvestIncrement 10 
      |> Resource.cooldown (25*Time.second))

  |> GameState.addResource
     (Resource.init "gold" 
      |> Resource.initialAmount 0 
      |> Resource.harvestIncrement 2
      |> Resource.cooldown (45*Time.second))


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
    |> trigger (Triggers.gameTimePassed (2 * Time.second))
    |> ln "You are cold..."
    |> ln "And starving."
    |> ln "Go search for some wood."
    |> mutator (Mutators.activateResource "wood")
  ,
    newEvent
    |> trigger (Triggers.resourceAbove "wood" 10)
    |> ln "Outside, you find a small heap of dry twigs."
    |> ln "Use them to start a fire."
  ,
    newEvent
    |> trigger Triggers.fireStoked
    |> reoccurring
    |> ln "The fire is roaring."
  ,
    newEvent
    |> trigger Triggers.fireStoked
    |> mutator (Mutators.setMilestone "fire-set-once")
  ,
    newEvent
    |> trigger (Triggers.timePassedSince "fire-set-once" (3*Time.second))
    |> ln "In the flames you see a warmth long forgotten..."
    |> ln "Don't let the fire go out."
  ,
    newEvent
    |> trigger (Triggers.and (Triggers.fireStoked)
                             (Triggers.milestoneReached "fire-set-once"))
    |> ln "You hear the hooting of the forest over the crackling flame."
    |> ln "You huddle close to the fire."
  ,
    newEvent
    |> trigger (Triggers.gameTimePassed (45 * Time.second))
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
    |> ln "Strangely, there was a bit of gold in its fur..."
    |> ln "Looks like you'll be hunting squirrels now."
    |> mutator (Mutators.and (Mutators.addToResource "gold" 10)
                             (Mutators.setMilestone "first-squirrel"))
  ,
    newEvent
    |> trigger (Triggers.and 
                  Triggers.fireExtinguished
                  (Triggers.resourceActive "gold"))
    |> ln "The fire is dead."
    |> ln "When the light comes back..."
    |> ln "Don't expect to find your gold."
    |> mutator (Mutators.setResourceAmount "gold" 0)
  ,
    newEvent
    |> trigger (Triggers.timePassedSince "first-squirrel" (10*Time.second))
    |> ln "Suddenly, you feel very cold."
    |> ln "Maybe murdering that squirrel is getting to you..."
  ]