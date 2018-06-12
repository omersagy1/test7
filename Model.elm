module Model exposing(..)

import Time exposing (Time)
import Queue.TimedQueue exposing(TimedQueue)
import Game.Model

type alias Model = 
  { currentPage : AppPage

  , editorModel : EditorModel
  , gameModel : Game.Model.Model
  }

type AppPage = EditorPage | GamePage

type alias EditorModel = 
  { val: Float
  , corpus: List String
  , display: List String
  , textDraft: String

  , gameTime: Time
  , paused: Bool
  , renderQueue : TimedQueue String
  }

type alias Display = List String

-- MESSAGES

type Message = EditorMessage EditorMessage
               | GameMessage Game.Model.Message
               | SwitchPage

type EditorMessage = UpdateTime Time
                     | AddText
                     | SaveDraft String
                     | Play
                     | Pause
