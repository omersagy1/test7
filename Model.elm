module Model exposing(..)

import Time exposing (Time)
import TimedQueue exposing(TimedQueue)

type alias Model = 
  { currentPage : AppPage

  , editorModel : EditorModel
  , gameModel : GameModel
  }

type AppPage = EditorPage | GamePage

type alias EditorModel = 
  { val: Float
  , corpus: List String
  , display: List String
  , textDraft: String

  , gameTime: Time
  , paused: Bool
  , renderQueue : TimedQueue.TimedQueue String
  }

type alias Display = List String


type alias GameModel = { dummy: Int }




-- MESSAGES

type Message = EditorMessage EditorMessage
               | GameMessage GameMessage
               | SwitchPage

type EditorMessage = UpdateTime Time
                     | AddText
                     | SaveDraft String
                     | Play
                     | Pause

type GameMessage = DummyMessage
