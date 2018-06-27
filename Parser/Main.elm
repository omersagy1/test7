module Parser.Main exposing (..)

import Time

import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource
import Game.Story as Story exposing (StoryEvent)
import Game.Condition as Condition exposing (..)

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
  -- |> GameState.addCustomAction
  --    (Action.init "hunt squirrels"
  --     |> Action.eff (Effect.chance 
  --                     .5 
  --                     (Effect.AddResource "gold" 5))
  --     |> Action.eff (Effect.DampenFire .8))


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
    |> trigger (GameTimePassed (1*Time.second))
    |> ln "You are cold..."
    |> ln "..."
    |> ln "Go search for some wood."
    |> effect (ActivateResource "wood")
  ,
    newEvent
    |> trigger (ResourceAmountAbove "wood" 10)
    |> ln "Outside, you find a small heap of dry twigs."
    |> ln "Use them to start a fire."
  ,
    newEvent
    |> trigger FireStoked
    |> reoccurring
    |> ln "The fire is roaring."
  ,
    newEvent
    |> trigger FireStoked
    |> effect (SetMilestoneReached "fire-set-once")
  ,
    newEvent
    |> trigger (TimeSinceMilestone "fire-set-once" (3*Time.second))
    |> ln "In the flames you see a warmth long forgotten..."
    |> ln "Don't let the fire go out."
  ,
    newEvent
    |> trigger (And FireStoked (MilestoneReached "fire-set-once"))
    |> ln "The flames are strong..."
    |> ln "But you're afraid, that you might get too used to the warmth."
  ,
    newEvent
    |> trigger (And FireStoked (MilestoneReached "fire-set-once"))
    |> ln "You hear the hooting of the forest over the crackling flame."
    |> ln "You huddle close to the fire."
  ,
    newEvent
    |> trigger (GameTimePassed (45*Time.second))
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
    |> effect (Compound2
               (AddToResource "gold" 10)
               (SetMilestoneReached "first-squirrel"))
  ,
    newEvent
    |> trigger (And FireExtinguished (ResourceActive "gold"))
    |> ln "The fire is dead."
    |> ln "When the light comes back..."
    |> ln "Don't expect to find your gold."
    |> effect (SetResourceAmount "gold" 0)
  ,
    newEvent
    |> trigger (TimeSinceMilestone "first-squirrel" (10*Time.second))
    |> ln "Suddenly, you feel very cold."
    |> ln "Maybe murdering that squirrel is getting to you..."
  ]