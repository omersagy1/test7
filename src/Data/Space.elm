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
      [ choice "meditate"
        |> consq (start
          |> ln "You breathe deeply."
          |> ln "The freezing air bites your face.")

      , choice "look up"
        |> consq (start
          |> ln "A thick haze clouds your vision."
          |> ln "But pale white letters speak to you..."
          |> ln "FUMIGATION CHAMBER")
      ]
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
  |> ln "The false starlight illuminates a rotting log."))

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
  |> ln "..."
  |> ln "There is something artificial about this place."
  |> effect (ActivateAction "investigate")))

  |> add (topLevel
  |> trigger (milestoneAtCount "did-investigate" 1)
  |> body (start
  |> restrict
  |> ln "The grass seems to stretch on for miles..."
  |> ln "And the stars look false."
  |> choices
      [ choice "call out"
        |> consq (start
          |> ln "Your own voice returns to you from every direction..."
          |> ln "Even from above.")
      , choice "stay silent"
        |> consq (start
            |> ln "The haze congeals on your face..."
            |> ln "But your eyes see better in silence."
            |> ln "The haze hangs heavily over the surrounding forest."
            |> ln "You return to the fire.")
      ]
  |> resume))

  |> add (topLevel
  |> trigger (milestoneAtCount "did-investigate" 2)
  |> body (start
  |> restrict
  |> ln "You find a body lying in the grass."
  |> ln "It's not moving."
  |> ln "The mud-stained dress on its back is speckled with flowers."
  |> choices 
      [ choice "turn it over"
        |> consq (start
        |> ln "A skull stares back up at you, framed by long blonde hair."
        |> ln "Its teeth are small and rotted."
        |> goto "defile-corpse-choice")
      , choice "crush its neck"
        |> consq (start
        |> ln "Its bony blonde head snaps from its shoulders..."
        |> ln "And rolls into the grass."
        |> ln "..."
        |> effect (SetMilestoneReached "corpse-defiled")
        |> goto "defile-corpse-choice")
      ]))

  |> add (topLevel
  |> name "defile-corpse-choice"
  |> body (start
  |> ln "Your hunger demands compense."
  |> choices
      [ choice "search body"
        |> consq (start
        |> ln "You rip off the corpse's dress..."
        |> ln "But only find an insect den among its fleshy bones."
        |> ln "There is nothing of value here."
        |> ln "Except the dress."
        |> effect (SetMilestoneReached "corpse-dress-taken"))
      , choice "return to fire"
        |> consq (start
        |> ln "You leave the weeping skull be."
        |> ln "The body fades into the haze..."
        |> ln "But not your memory of its flowery dress.")
      ]
  |> resume))

  |> add (topLevel
    |> trigger (milestoneAtCount "did-investigate" 3)
    |> body (start
    |> restrict
    |> ln "It's raining..."
    |> ln "The drops sting your body."
    |> ln "You go back inside."
    |> ln "There is a someone sitting by the fire."
    |> choices
        [ choice "Who are you?"
          |> consq (start
          |> ln "..."
          |> ln "The man looks up."
          |> di "Didn't expect to ever find another soul up here on AL 50."
          |> di "Except for ol' Cathy."
          |> cond (milestoneReached "corpse-dress-taken")
              (speak "...whose dress I see you've taken to wearing."
               |> di "An original look, I can give you that."
               |> ln "The man chuckles to himself.")
          |> cond (milestoneReached "corpse-defiled")
              (speak "Poor girl's head came right off her poor head."
               |> di "Wonder what kind of nasty critter would do something like that...")
          |> goto "visitor-intro")
        , choice "Get out!"
          |> consq (start
          |> ln "..."
          |> ln "The man is unfazed."
          |> di "You ought to relax, stranger."
          |> cases (caseList
              |> caseif (milestoneReached "corpse-dress-taken")
                  (speak "I ain't afraid of a haggard man in a flowery dress.")
              |> caseif (milestoneReached "corpse-defiled")
                  (speak "You aren't going to be able to crush my bony head like you did poor Cathy's."))
          |> di "Ain't nobody up here on AL 50 except me..."
          |> di "And you."
          |> goto "visitor-intro")
        ]))
  
  |> add (topLevel
  |> name "visitor-intro"
  |> body (start
  |> ln "The man reaches out his hand."
  |> ln "You shake it."
  |> di "You look like you need some food, friend."
  |> di "Have a critter."
  |> ln "The man plops a dead rat into your trembling palm."
  |> effect (AddToResource "rats" 1)
  |> di "I tell you I never seen rats on the whole Ark like I seen 'em here."
  |> di "Name's Don."
  |> resume
  |> di "Mine, not the rat's."
  |> effect (ActivateAction "hunt rats")))

  |> add (topLevel
  |> trigger (milestoneAtCount "rats-hunted" 1)
  |> body (start
  |> ln "You scramble in the mud and dig your nails into a writhing mass."))

  |> add (topLevel
  |> trigger (timeSinceMilestone "rats-hunted" (10*Time.second))
  |> body (start
  |> goto "game-over"))
  
  |> add (topLevel
  |> name "game-over"
  |> body (start
  |> ln "..."
  |> ln " ~ TO BE CONTINUED ~ "
  |> effect GameOver))


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

  |> GameState.addCustomAction
      (Action.init "hunt rats"
        |> Action.cooldown (60*Time.second)
        |> Action.effect (Compound2 (AddToResourceRand "rats" 1 2)
                                    (IncrementMilestone "rats-hunted")))