module Data.Space exposing (story, init)

import Time

import Game.Action as Action
import Game.Effect as Effect exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Resource as Resource
import Game.Story exposing (..)
import Parser.Build exposing (..)
import Parser.Condition exposing (..)


story : Story
story =
  begin
  |> add (topLevel
    |> trigger (gameTimePassed (1.5*Time.second))
    |> body (start
    |> ln "The room is cold."
    |> ln "Foul water drips from the ceiling."
    |> ln "..."
    |> ln "You shiver."
    |> choices 
        [ choice "Shut eyes"
          |> consq (start
            |> ln "You breathe deeply."
            |> ln "The freezing air bites your face."
            |> goto "need-a-light")

        , choice "Look up"
          |> consq (start
            |> ln "A thick haze clouds your vision."
            |> ln "But pale white letters speak to you..."
            |> ln "DEFUMIGATION CHAMBER"
          |> goto "need-a-light")
        ]))

  |> add (topLevel
    |> name "need-a-light"
    |> body (start
    |> ln "..."
    |> ln "The cold is unbearable."
    |> effect (ActivateAction "search for wood")))

  |> add (topLevel
    |> trigger (and (actionPerformed "search for wood")
                    (milestoneAtCount "wood-searched" 1))
    |> body (start
    |> ln "The ground is covered in grass..."
    |> ln "And dry little twigs."
    |> ln "They're meager, but they'll feed a fire."))

  |> add (topLevel
    |> trigger fireStoked
    |> body (start
    |> ln "Embers hatch in the brush."
    |> ln "You hover your hands over the flame."
    |> ln "..."
    |> ln "The fire is roaring."
    |> effect (IncrementMilestone "fire-stoked")))
  
  |> add (topLevel
    |> trigger (and fireStoked (milestoneAtCount "fire-stoked" 1))
    |> body (start
    |> ln "You bring your face close to the flames."
    |> ln "A foreign smile stretches your lips."
    |> effect (IncrementMilestone "fire-stoked")))

  |> add (topLevel
    |> reoccurring
    |> trigger (and fireStoked (milestoneGreaterThan "fire-stoked" 1))
    |> body (start
    |> rand [ narrate "The fire is roaring."
            , narrate "The fire is roaring."
            , narrate "The fire is roaring."
            , narrate "The flames double in height."
            , narrate "The fire crackles with a human voice."
            , narrate "You shiver with warmth."
            , narrate "The flames rise."
            , narrate "The fire dances with primitive urgency."
            ]
    |> effect (IncrementMilestone "fire-stoked")))

  |> add (topLevel
    |> trigger (milestoneAtCount "wood-searched" 2)
    |> body (start
    |> ln "You feel around in the dark."
    |> ln "The distant fire shines a small light on a rotting log."))

  |> add (topLevel
    |> reoccurring
    |> trigger (and (milestoneGreaterThan "wood-searched" 2)
                    (actionPerformed "search for wood"))
    |> body (start
    |> rand 
        [ narrate "A few more twigs for the fire..."
        , narrate "The twigs are dirty, but dry enough..."
        , narrate "From your dark wandering you return with a small bounty..."
        ]))

  |> add (topLevel
    |> trigger (timeSinceMilestone "fire-stoked" (5*Time.second))
    |> body (start
    |> ln "There is something artificial about this place."
    |> effect (ActivateAction "investigate")))

  |> add (topLevel
    |> trigger (milestoneAtCount "did-investigate" 1)
    |> body (start
    |> ln "The grass seems to stretch on for miles..."
    |> ln "And the stars look false."
    |> choices
        [ choice "Yell"
          |> consq (start
            |> ln "Your own voice returns to you from every direction..."
            |> ln "Even from above.")
        , choice "Stay Silent"
          |> consq (start
              |> ln "The haze congeals on your face."
              |> ln "You return to the fire.")
        ]))

  |> add (topLevel
    |> trigger (milestoneAtCount "did-investigate" 2)
    |> body (start
    |> ln "You find a body lying in the grass."
    |> ln "It's not moving."
    |> choices 
        [ choice "Turn it over"
          |> consq (start
              |> ln "A skull stares back up at you, framed by long blonde hair."
              |> ln "Its teeth are small and rotted."
              |> ln "On its body is nothing of value.")
        , choice "Crush its neck"
          |> consq (start
              |> ln "Its bony head snaps from its shoulders..."
              |> ln "And rolls into the grass."
              |> ln "..."
              |> ln "You rip off the body's dress but find nothing of value."
              |> effect (SetMilestoneReached "corpse-defiled"))
        ]))

  |> add (topLevel
    |> trigger (milestoneAtCount "did-investigate" 3)
    |> body (start
    |> ln "It's raining..."
    |> ln "The drops sting your body."
    |> ln "You go back inside."
    |> ln "There is a someone sitting by the fire."
    |> choices
        [ choice "Who are you?"
          |> consq (start
            |> ln "..."
            |> ln "The man looks up."
            |> ln "'It's been years since I've seen anyone else on the starboard.'"
            |> ln "'Except for ol' Cathy.'"
            |> cond 
                (milestoneReached "corpse-defiled")
                (narrate "'...whose poor neck you decided to crush.'"
                  |> ln "The man chuckles to himself.")
            |> goto "visitor-intro")
        , choice "Get out!"
          |> consq (start
            |> ln "..."
            |> ln "The man is unfazed."
            |> ln "'You ought to relax, stranger.'"
            |> cases (caseList
               |> caseif 
                   (milestoneReached "corpse-defiled")
                   (narrate "'You aren't going to be able to crush my head like you did ol' Cathy's.'"
                    |> goto "game-over")
               |> default
                   (narrate "'It's just you and me out here on the starboard.'"
                    |> ln "'Been years and years since I seen anyone else.'"
                    |> ln "'Damn near forgot how to talk.'"
                    |> ln "He smiles."
                    |> goto "visitor-intro")
                ))
        ]))
  
  |> add (topLevel
    |> name "visitor-intro"
    |> body (start
    |> ln "The man reaches out his hand."
    |> ln "You shake it."
    |> goto "game-over"))
  
  |> add (topLevel
    |> name "game-over"
    |> body (start
    |> ln "..."
    |> ln " ~ to be continued..."
    |> effect (Compound [ (DeactivateAction "search for wood")
                        , (DeactivateAction "investigate")
                        ])))


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
        |> Action.cooldown (40*Time.second)
        |> Action.effect (IncrementMilestone "did-investigate"))