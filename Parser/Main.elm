module Parser.Main exposing (..)

import Time

import Game.Action as Action
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
  |> GameState.addResource (Resource.init "wood" 0)
  |> GameState.addResource (Resource.init "rats" 0)
  |> GameState.addResource (Resource.init "gold" 0)
  |> GameState.addCustomAction
      (Action.init "search for wood"
        |> Action.cooldown (30*Time.second)
        |> Action.effect (AddToResource "wood" 5))
  |> GameState.addCustomAction
      (Action.init "hunt rats"
        |> Action.effect (AddToResource "rats" 2)
        |> Action.cooldown (60*Time.second))


storyEventCorpus : List StoryEvent
storyEventCorpus = 
  [ newEvent
    |> trigger (GameTimePassed (1.5*Time.second))
    |> ln "The fire is dead."
    |> ln "The room is freezing."
    |> ln "..."
    |> ln "You need to find wood."
    |> effect (ActivateAction "search for wood")
  ,
    newEvent
    |> trigger (CustomActionPerformed "search for wood")
    |> ln "Outside, you find a small heap of dry twigs."
    |> ln "Use them to start a fire."
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (And (CustomActionPerformed "search for wood")
                    (And (MilestoneReached "wood-searched")
                         (Not (MilestoneAtCount "wood-searched" 2))))
    |> reoccurring
    |> ln "A few more twigs for the fire..."
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (And (CustomActionPerformed "search for wood")
                    (MilestoneAtCount "wood-searched" 2))
    |> ln "The infinite murmur of the forest envelops you..."
    |> ln "You hear the sound of a gay deer in the distance." 
    |> effect (IncrementMilestone "wood-searched")
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
    |> trigger (And FireExtinguished (ResourceActive "gold"))
    |> ln "The fire is dead."
    |> ln "When the light comes back..."
    |> ln "You will only find an empty room."
    |> effect (SetResourceAmount "wood" 0)
    |> effect (SetResourceAmount "rats" 0)
    |> effect (SetResourceAmount "gold" 0)
    ,
    newEvent
    |> trigger (GameTimePassed (35*Time.second))
    |> ln "Your belly rolls with hunger."
    |> ln "And in the corner, you find a dead rat."
    |> choice
        (newChoice 
         |> text "Eat it"
         |> consq 
             (newEvent
              |> ln "You hold it over the flame..."
              |> ln "And bite into it just as its skin begins to sizzle."
              |> ln "There is something holy about its taste."
              |> ln "You want more."))
    |> choice
        (newChoice
         |> text "Keep it "
         |> consqName "rat-kept")
  ,
    newEvent
    |> name "rat-kept"
    |> ln "You hold the rat tight in your hand..."
    |> ln "A greed overtakes you."
    |> ln "You need more rats."
    |> effect (Compound
               [ AddToResource "rats" 1
               , SetMilestoneReached "first-rat"
               , ActivateAction "hunt rats" ])
  ,
    newEvent
    |> trigger (TimeSinceMilestone "first-squirrel" (10*Time.second))
    |> ln "Suddenly, you feel very cold."
    |> ln "Maybe murdering that squirrel is getting to you..."
  ]