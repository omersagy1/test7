module Data.Simple exposing (init, corpus)

import Time exposing (Time)

import Game.GameState as GameState exposing (GameState)
import Game.Story as Story exposing (StoryEvent)
import Parser.Build exposing (..)
import Parser.Choice as Chc
import Parser.Consequence as Csq
import Parser.Condition exposing (..)

init : GameState
init = GameState.init 

corpus : List StoryEvent
corpus =
  [ newEvent
    |> trigger (gameTimePassed (2*Time.second))
    |> choice 
        (Chc.new
         |> Chc.text "Run"
         |> Chc.consq 
            (Csq.new
             |> Csq.cond (chance 0.5)  
             |> Csq.event
                (newEvent
                 |> ln "You trip and fall!"))
         |> Chc.consq 
            (Csq.new
             |> Csq.event
                (newEvent
                 |> ln "You got away!")))
    |> choice 
        (Chc.new
         |> Chc.text "Hide"
         |> Chc.consq 
            (Csq.new
             |> Csq.cond (chance 0.5)  
             |> Csq.event
                (newEvent
                 |> ln "You're found out!")))
  ]