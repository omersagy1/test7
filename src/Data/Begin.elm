module Data.Begin exposing (corpus)

import Time

import Game.Effect as Effect exposing (..)
import Game.Story as Story exposing (StoryEvent)

import Parser.Build exposing (..)
import Parser.Condition exposing (..)

corpus : List StoryEvent
corpus = 
  [ newEvent
    |> trigger (gameTimePassed (1.5*Time.second))
    |> ln "The fire is dead."
    |> ln "The room is freezing."
    |> ln "..."
    |> ln "You'll die of exposure if you can't find drywood."
    |> effect (ActivateAction "search for wood")
  ,
    newEvent
    |> trigger (actionPerformed "search for wood")
    |> ln "Outside, you find a small heap of twigs."
    |> ln "They're meager, but they'll feed a fire."
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (and (actionPerformed "search for wood")
                    (and (milestoneReached "wood-searched")
                         (fail (milestoneAtCount "wood-searched" 2))))
    |> reoccurring
    |> randlns 
        [ "A few more twigs for the fire..."
        , "The twigs are dirty, but dry enough..."
        , "You return from your dark wandering with a small bounty..."
        ]
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (and (actionPerformed "search for wood")
                    (milestoneAtCount "wood-searched" 2))
    |> ln "The infinite murmur of the forest envelops you..."
    |> ln "In the distance, you can hear the sound of a gay deer." 
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger fireStoked
    |> ln "A spark turns to flame, and you sit back as your body thaws."
    |> ln "The fire is roaring."
    |> effect (IncrementMilestone "fire-stoked")
  ,
    newEvent
    |> trigger (and fireStoked (milestoneGreaterThan "fire-stoked" 0))
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
    |> effect (IncrementMilestone "fire-stoked")
  ,
    newEvent
    |> trigger (timeSinceMilestone "fire-set-once" (3*Time.second))
    |> ln "In the flames you see a warmth long forgotten..."
    |> ln "Don't let the fire go out."
  ,
    newEvent
    |> trigger (and fireExtinguished (milestoneReached "fire-set-once"))
    |> ln "The fire is dead."
    |> ln "When the light comes back..."
    |> ln "Expect the worst."
    |> effect (Compound 
                [ SetResourceAmount "wood" 0
                , SetResourceAmount "rats" 0
                , SetResourceAmount "gold" 0
                ])
    ,
    newEvent
    |> trigger (timeSinceMilestone "fire-stoked" (25*Time.second))
    |> ln "Your belly rolls with hunger."
    |> ln "In the corner, you find a dead rat."
    |> choice
        (newChoice 
         |> text "Eat it"
         |> consq 
             (newEvent
              |> ln "You hold it over the flame..."
              |> ln "And bite into it just as its skin begins to sizzle."
              |> ln "There is something holy about its taste."
              |> effect (SetMilestoneReached "first-rat-eaten")
              |> goto "more-rats"))
    |> choice
        (newChoice
         |> text "Keep it "
         |> consqName "rat-kept")
  ,
    newEvent
    |> name "rat-kept"
    |> ln "You hold the rat tight in your hand..."
    |> ln "A greed overtakes you."
    |> goto "more-rats"
  ,
    newEvent
    |> name "more-rats"
    |> ln "You need more rats."
    |> effect (Compound
               [ AddToResource "rats" 1
               , SetMilestoneReached "first-rat"
               , ActivateAction "hunt rats" ])
  ,
    newEvent
    |> trigger (actionPerformed "hunt rats")
    |> ln "You scramble in the firelight, and feel sick with yourself."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (and (actionPerformed "hunt rats")
                    (milestoneAtCount "hunting-rats" 1))
    |> ln "You no longer close your eyes when you smash their skulls."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (and (actionPerformed "hunt rats")
                    (milestoneGreaterThan "hunting-rats" 1))
    |> ln "You catch the rodents quietly and automatically."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (timeSinceMilestone "first-rat-eaten" (20*Time.second))
    |> ln "You feel very cold..."
    |> ln "From your mouth comes a glob of vomit speckled with rat bones."
  ]