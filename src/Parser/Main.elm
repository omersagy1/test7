module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (Story)

import Data.Init
import Data.Begin
import Data.Simple
import Data.Space


initialGameState : GameState
initialGameState = Data.Space.init

storyEventCorpus : Story
storyEventCorpus = Data.Space.story