module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (StoryEvent)

import Data.Init
import Data.Begin
import Data.Simple


initialGameState : GameState
initialGameState = Data.Init.init

storyEventCorpus : List StoryEvent
storyEventCorpus = Data.Begin.corpus