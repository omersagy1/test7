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
        |> Action.cooldown (25*Time.second)
        |> Action.effect (AddToResource "wood" 15))
  |> GameState.addCustomAction
      (Action.init "catch rats"
        |> Action.effect (AddToResource "rats" 2)
        |> Action.cooldown (40*Time.second))
  |> GameState.addCustomAction
      (Action.init "hunt squirrels"
        |> Action.effect (AddToResource "gold" 5)
        |> Action.cooldown (70*Time.second))


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
    |> effect (Compound
               [ AddToResource "gold" 10
               , SetMilestoneReached "first-squirrel"
               , ActivateAction "hunt squirrels" ])
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