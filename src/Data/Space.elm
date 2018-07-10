module Data.Space exposing (story, init)

import Time

import Game.Action as Action
import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource
import Game.Story as Story exposing (Story)
import Parser.Build exposing (..)
import Parser.Condition exposing (..)


story : Story
story =
  begin
  |> add 
     (newEvent
      |> trigger (gameTimePassed (1.5*Time.second))
      |> ln "The room is cold."
      |> ln "..."
      |> ln "Foul water drips from the ceiling."
      |> ln "You shiver."
      |> choice
         (newChoice
          |> text "Shut eyes"
          |> directConsq 
             (newEvent
              |> ln "You breathe deeply."
              |> ln "The freezing air bites your face."
              |> goto "need-a-light"))
      |> choice
         (newChoice
          |> text "Look up"
          |> directConsq 
             (newEvent
              |> ln "A thick haze clouds your vision."
              |> ln "But you swear the pale white letters speak to you..."
              |> ln "DEFUMIGATION CHAMBER"
              |> ln "..."
              |> ln "The cold is unbearable."
              |> goto "need-a-light")))
  |> add
     (newEvent
      |> name "need-a-light"
      |> ln "..."
      |> ln "Feeling returns to your hands..."
      |> ln "The ground is covered in grass."
      |> ln "..."
      |> ln "And dry little twigs."
      |> effect (ActivateAction "search for wood"))


init : GameState
init =
  GameState.init 
  |> GameState.initFire (30*Time.second) (10*Time.second)
  |> GameState.addResource (Resource.init "wood" 0)
  |> GameState.addResource (Resource.init "rats" 0)
  |> GameState.addResource (Resource.init "gold" 0)

  |> GameState.addCustomAction
      (Action.init "search for wood"
        |> Action.cooldown (30*Time.second)
        |> Action.effect (AddToResourceRand "wood" 2 4))

  |> GameState.addCustomAction
      (Action.init "hunt rats"
        |> Action.effect (AddToResource "rats" 2)
        |> Action.cooldown (60*Time.second))