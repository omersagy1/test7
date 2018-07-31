module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (Story)

-- import Data.Simple
import Data.Space


initialGameState : GameState
initialGameState = Data.Space.init

story : Story
story = Data.Space.story