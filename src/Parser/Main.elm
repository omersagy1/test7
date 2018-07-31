module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (Story)

import Data.Simple
import Data.Space


initialGameState : GameState
initialGameState = Data.Simple.init

story : Story
story = Data.Simple.story