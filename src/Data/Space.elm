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
          |> goto "need-a-light")))

  |> add (newEvent
    |> name "need-a-light"
    |> ln "..."
    |> ln "The cold is unbearable."
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
    |> reoccurring
    |> trigger (and fireStoked (milestoneGreaterThan "fire-stoked" 1))
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
    |> trigger (milestoneAtCount "wood-searched" 2)
    |> ln "You feel around in the dark."
    |> ln "The distant fire shines a small light on a rotting log.")

  |> add (newEvent
    |> reoccurring
    |> trigger (and (milestoneGreaterThan "wood-searched" 2)
                    (actionPerformed "search for wood"))
    |> randlns 
        [ "A few more twigs for the fire..."
        , "The twigs are dirty, but dry enough..."
        , "From your dark wandering you return with a small bounty..."
        ])

  |> add (newEvent
    |> trigger (timeSinceMilestone "fire-stoked" (5*Time.second))
    |> ln "There is something artificial about this place."
    |> effect (ActivateAction "investigate"))

  |> add (newEvent
    |> trigger (milestoneAtCount "did-investigate" 1)
    |> ln "The grass seems to stretch on for miles..."
    |> ln "And the stars look false."
    |> choice (newChoice
      |> text "Yell"
      |> directConsq (newEvent
        |> ln "Your own voice returns to you from every direction..."
        |> ln "Even from above."))
    |> choice (newChoice
      |> text "Stay Silent"
      |> directConsq (newEvent
        |> ln "The haze congeals on your face."
        |> ln "You return to the fire.")))

  |> add (newEvent
    |> trigger (milestoneAtCount "did-investigate" 2)
    |> ln "You find a body lying in the grass."
    |> ln "It's not moving."
    |> choice (newChoice
      |> text "Turn it over"
      |> directConsq (newEvent
        |> ln "A skull stares back up at you, framed by long blonde hair."
        |> ln "Its teeth are small and rotted."
        |> ln "On its body is nothing of value."))
    |> choice (newChoice
      |> text "Crush its neck"
      |> directConsq (newEvent
        |> ln "Its bony head snaps from its shoulders..."
        |> ln "And rolls into the grass."
        |> ln "..."
        |> ln "You rip off the body's dress but find nothing of value."
        |> effect (SetMilestoneReached "corpse-defiled"))))

  |> add (newEvent
    |> trigger (milestoneAtCount "did-investigate" 3)
    |> ln "It's raining..."
    |> ln "The drops sting your body."
    |> ln "You go back inside."
    |> ln "There is a someone sitting by the fire."
    |> choice (newChoice
      |> text "Who are you?"
      |> directConsq (newEvent
        |> ln "..."
        |> ln "The man looks up."
        |> ln "'It's been years since I've seen anyone else on the starboard.'"
        |> ln "'Except for ol' Cathy.'"
        |> subsq (newConsq
          |> cond (milestoneReached "corpse-defiled")
          |> ref "comment-on-defiling")
        |> subsq (newConsq
          |> ref "visitor-intro")))
    |> choice (newChoice
      |> text "Get out!"
      |> directConsq (newEvent
        |> ln "..."
        |> ln "The man is unfazed."
        |> ln "'You ought to relax, stranger.'"
        |> subsq (newConsq
          |> cond (milestoneReached "corpse-defiled")
          |> ref "threat-on-defiling")
        |> subsq (newConsq
          |> ref "stranger-relax"))))

  |> add (newEvent
    |> name "comment-on-defiling"
    |> ln "..."
    |> ln "The man chuckles to himself."
    |> ln "'...whose poor neck you decided to crush.'"
    |> goto "visitor-intro")

  |> add (newEvent
    |> name "threat-on-defiling"
    |> ln "'You aren't going to be able to crush my head like you did ol' Cathy's.'"
    |> goto "visitor-intro")

  |> add (newEvent
    |> name "stranger-relax"
    |> ln "'It's just you and me out here on the starboard.'"
    |> ln "'Been years and years since I seen anyone else.'"
    |> ln "'Damn near forgot how to talk.'"
    |> ln "He smiles."
    |> goto "visitor-intro")
  
  |> add (newEvent
    |> name "visitor-intro"
    |> ln "The man reaches out his hand."
    |> ln "You shake it."
    |> goto "game-over")
  
  |> add (newEvent
    |> name "game-over"
    |> ln "..."
    |> ln " ~ to be continued..."
    |> effect (Compound [ (DeactivateAction "search for wood")
                        , (DeactivateAction "investigate")
                        ]))


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