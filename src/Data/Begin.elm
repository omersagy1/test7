module Data.Begin exposing (corpus)

import Time

import Game.Effect as Effect exposing (..)
import Game.Story as Story exposing (StoryEvent)
import Game.Condition as Condition exposing (..)

import Parser.Build exposing (..)

corpus : List StoryEvent
corpus = 
  [ newEvent
    |> trigger (GameTimePassed (1.5*Time.second))
    |> ln "The fire is dead."
    |> ln "The room is freezing."
    |> ln "..."
    |> ln "You'll die of exposure if you can't find drywood."
    |> effect (ActivateAction "search for wood")
  ,
    newEvent
    |> trigger (ActionPerformed "search for wood")
    |> ln "Outside, you find a small heap of twigs."
    |> ln "They're meager, but they'll feed a fire."
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (And (ActionPerformed "search for wood")
                    (And (MilestoneReached "wood-searched")
                         (Not (MilestoneAtCount "wood-searched" 2))))
    |> reoccurring
    |> ln "A few more twigs for the fire..."
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger (And (ActionPerformed "search for wood")
                    (MilestoneAtCount "wood-searched" 2))
    |> ln "The infinite murmur of the forest envelops you..."
    |> ln "In the distance, you can hear the sound of a gay deer." 
    |> effect (IncrementMilestone "wood-searched")
  ,
    newEvent
    |> trigger FireStoked
    |> ln "A spark turns to flame, and you sit back as your body thaws."
    |> ln "The fire is roaring."
    |> effect (IncrementMilestone "fire-stoked")
  ,
    newEvent
    |> trigger (And FireStoked (MilestoneGreaterThan "fire-stoked" 0))
    |> reoccurring
    |> ln "The fire is roaring."
    |> effect (IncrementMilestone "fire-stoked")
  ,
    newEvent
    |> trigger (TimeSinceMilestone "fire-set-once" (3*Time.second))
    |> ln "In the flames you see a warmth long forgotten..."
    |> ln "Don't let the fire go out."
  ,
    newEvent
    |> trigger (And FireExtinguished (MilestoneReached "fire-set-once"))
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
    |> trigger (TimeSinceMilestone "fire-stoked" (25*Time.second))
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
    |> trigger (ActionPerformed "hunt rats")
    |> ln "You scramble in the firelight, and feel sick with yourself."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (And (ActionPerformed "hunt rats")
                    (MilestoneAtCount "hunting-rats" 1))
    |> ln "You no longer close your eyes when you smash their skulls."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (And (ActionPerformed "hunt rats")
                    (MilestoneGreaterThan "hunting-rats" 1))
    |> ln "You catch the rodents quietly and automatically."
    |> effect (IncrementMilestone "hunting-rats")
  ,
    newEvent
    |> trigger (TimeSinceMilestone "first-rat-eaten" (20*Time.second))
    |> ln "You feel very cold..."
    |> ln "From your mouth comes a glob of vomit speckled with rat bones."
  ]