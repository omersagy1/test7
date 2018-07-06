module Data.Simple exposing (init, corpus)

import Time exposing (Time)

import Game.Condition as Condition exposing (..)
import Game.GameState as GameState exposing (GameState)
import Game.Story as Story exposing (StoryEvent)
import Parser.Build exposing (..)

init : GameState
init = GameState.init 

corpus : List StoryEvent
corpus =
  [ newEvent
    |> ln "builds!"
  ]