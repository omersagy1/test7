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

  |> add (newEvent
    |> trigger (gameTimePassed (1.5*Time.second))
    |> ln "The room is cold."
    |> ln "Foul water drips from the ceiling."
    |> ln "..."
    |> ln "You shiver."
    |> choice (newChoice
       |> text "Shut eyes"
       |> directConsq (newEvent
          |> ln "You breathe deeply."
          |> ln "The freezing air bites your face."
          |> goto "need-a-light"))
    |> choice (newChoice
       |> text "Look up"
       |> directConsq (newEvent
          |> ln "A thick haze clouds your vision."
          |> ln "But pale white letters speak to you..."
          |> ln "DEFUMIGATION CHAMBER"
          |> ln "..."
          |> ln "The cold is unbearable."
          |> goto "need-a-light")))

  |> add (newEvent
    |> name "need-a-light"
    |> effect (ActivateAction "search for wood"))

  |> add (newEvent
    |> trigger (and (actionPerformed "search for wood")
                    (milestoneAtCount "wood-searched" 1))
    |> ln "The ground is covered in grass..."
    |> ln "And dry little twigs."
    |> ln "They're meager, but they'll feed a fire.")

  |> add (newEvent
    |> trigger fireStoked
    |> ln "Embers hatch in the brush."
    |> ln "You hover your hands over the flame."
    |> ln "..."
    |> ln "The fire is roaring."
    |> effect (IncrementMilestone "fire-stoked"))
  
  |> add (newEvent
    |> trigger (and fireStoked (milestoneAtCount "fire-stoked" 1))
    |> ln "You bring your face close to the flames."
    |> ln "A foreign smile breaks upon your face."
    |> effect (IncrementMilestone "fire-stoked"))

  |> add (newEvent
    |> trigger (and fireStoked (milestoneGreaterThan "fire-stoked" 1))
    |> reoccurring
    |> randlns [ "The fire is roaring."
               , "The fire is roaring."
               , "The fire is roaring."
               , "The flames double in height."
               , "The fire crackles with a human voice."
               , "You shiver with warmth."
               , "The flames rise."
               , "The fire dances with primitive urgency."
               ]
    |> effect (IncrementMilestone "fire-stoked"))
  
  |> add (newEvent
    |> trigger (and (actionPerformed "search for wood")
                    (milestoneAtCount "wood-searched" 2))
    |> ln "You feel around in the dark."
    |> ln "The distant fire shines a small light on a rotting log.")

  |> add (newEvent
    |> trigger (timeSinceMilestone "fire-stoked" (5*Time.second))
    |> ln "There is something artificial about this place."
    |> effect (ActivateAction "investigate"))

  |> add (newEvent
    |> trigger (milestoneAtCount "did-investigate" 1)
    |> ln "The grass seems to stretch on for miles..."
    |> ln "And the stars look false.")

init : GameState
init =
  GameState.init 
  |> GameState.initFire (50*Time.second) (20*Time.second)
  |> GameState.addResource (Resource.init "wood" 0)
  |> GameState.addResource (Resource.init "rats" 0)
  |> GameState.addResource (Resource.init "gold" 0)

  |> GameState.addCustomAction
      (Action.init "search for wood"
        |> Action.cooldown (30*Time.second)
        |> Action.effect (Compound2 (AddToResourceRand "wood" 2 4)
                                    (IncrementMilestone "wood-searched")))

  |> GameState.addCustomAction
      (Action.init "investigate"
        |> Action.cooldown (60*Time.second)
        |> Action.effect (IncrementMilestone "did-investigate"))