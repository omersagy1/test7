module Data.Simple exposing (init, story)

import Time exposing (Time)

import Game.Action as Action
import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource
import Game.Story exposing (..)
import Parser.Build exposing (..)
import Parser.Condition exposing (..)

init : GameState
init = GameState.init 
  |> GameState.initFire (50*Time.second) (20*Time.second)
  |> GameState.addResource (Resource.init "wood" 0)
  |> GameState.addCustomAction
      (Action.init "search for wood"
        |> Action.cooldown (30*Time.second)
        |> Action.effect (Compound2 (AddToResourceRand "wood" 2 4)
                                    (IncrementMilestone "wood-searched")))

story : Story
story = begin

  |> add (topLevel 
    |> name "first"
    |> trigger (gameTimePassed (1*Time.second))
    |> body (start
      |> rand [ narrate "hello1"
              , narrate "hello2"
              , narrate "hello3"
              , narrate "hello4"
              , narrate "hello5"
              ]
      |> ln "world"
      |> effect (ActivateAction "search for wood")
      |> ln "activated..."
      |> effect (SetMilestoneReached "t1")
      |> cond (gameTimePassed (0.8*Time.second))
              (narrate "this one shows!")
      |> cond (milestoneReached "fake") 
              (narrate "this one doesn't!")
      |> choices
          [ choice "yes" 
            |> consq (start
              |> ln "said yes!"
              |> goto "reffed")

          , choice "no" 
            |> consq (narrate "said no...")

          , choice "maybe?"
            |> condition (gameTimePassed (0.5*Time.second))
            |> consq (narrate "was available...")

          , choice "never"
            |> condition (milestoneReached "fake")
            |> consq (narrate "never seen..")
          ]
      ))
  
  |> add (topLevel
    |> trigger fireStoked
    |> reoccurring
    |> body (start
      |> ln "the fire is roaring."))
  
  |> add (topLevel
    |> name "reffed"
    |> body (start
      |> ln "got to the referenced event!"))