module Data.Simple exposing (init, story)

import Time exposing (Time)

import Game.Action as Action
import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource
import Game.Story exposing (..)
import Game.StoryEvent exposing (..)
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
      |> ln "hello"
      |> ln "world"
      |> effect (ActivateAction "search for wood")))
  
  |> add (topLevel
    |> trigger fireStoked
    |> reoccurring
    |> body (start
      |> ln "the fire is roaring."))