module Data.Simple exposing (init, corpus)

import Time exposing (Time)

import Game.GameState as GameState exposing (GameState)
import Game.Story as Story exposing (StoryEvent)
import Parser.Build exposing (..)
import Parser.Condition exposing (..)

init : GameState
init = GameState.init 

corpus : List StoryEvent
corpus =
  [ newEvent
    |> trigger (gameTimePassed (2*Time.second))
    |> choice 
        (newChoice
         |> text "Run"
         |> consq 
            (newConsq
             |> cond (chance 0.5)  
             |> event
                (newEvent
                 |> ln "You trip and fall!"))
         |> consq 
            (newConsq
             |> event
                (newEvent
                 |> ln "You got away!")))
    |> choice 
        (newChoice
         |> text "Hide"
         |> consq 
            (newConsq
             |> cond (chance 0.5)  
             |> event
                (newEvent
                 |> ln "You're found out!"))
         |> consq
            (newConsq
             |> event
                (newEvent
                 |> ln "You stay undiscovered.")))
  ]