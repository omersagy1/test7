module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (StoryEvent)

import Data.Init
import Data.Begin


initialGameState : GameState
initialGameState = Data.Init.initialGameState

storyEventCorpus : List StoryEvent
storyEventCorpus = 
  Data.Begin.corpus