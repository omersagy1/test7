module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (Story)

import Data.Space
import Data.Simple


initialGameState : GameState
initialGameState = Data.Space.init

story : Story
story = Data.Space.story