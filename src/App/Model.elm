module App.Model exposing(..)

import Editor.Main
import Game.Model
import Game.Update

type alias Model = 
  { currentPage : AppPage
  , editorModel : Editor.Main.Model
  , gameModel : Game.Model.Model
  }

type AppPage = EditorPage | GamePage

-- MESSAGES

type Message = EditorMessage Editor.Main.Message
               | GameMessage Game.Update.Message
               | SwitchPage
