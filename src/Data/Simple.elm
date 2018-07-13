module Data.Simple exposing (init, story)

import Time exposing (Time)

import Game.GameState as GameState exposing (GameState)
import Game.Story exposing (..)
import Game.StoryEvent exposing (..)
import Parser.Condition exposing (..)

init : GameState
init = GameState.init 

story : Story
story = begin
  |> add (topLevel 
    |> name "first"
    |> trigger unconditionally
    |> body (start
      |> ln "hello"
      |> ln "world"))