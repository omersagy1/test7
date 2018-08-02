module Game.Message exposing (..)

import Time exposing (Time)

import Game.ActionName as ActionName
import Game.StoryEvent exposing (Choice)


-- Messages to control the running of the game
type Message = TogglePause
               | ToggleFastForward
               | Restart
               | UpdateTime Time
               | MakeChoice Choice
               | GameplayMessage ActionName.Name
               | StartTime Time