module Parser.Main exposing (..)

import Game.GameState exposing (GameState)
import Game.Story exposing (StoryEvent)

-- import Data.Init
-- import Data.Begin
import Data.Simple


initialGameState : GameState
initialGameState = Data.Simple.init

storyEventCorpus : List StoryEvent
storyEventCorpus = Data.Simple.corpus