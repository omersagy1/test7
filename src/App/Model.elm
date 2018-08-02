module App.Model exposing(..)

import Navigation

import Editor.Main
import Game.Model
import Game.Message


type alias Model = 
  { currentPage : AppPage
  , editorModel : Editor.Main.Model
  , gameModel : Game.Model.Model
  }

type AppPage = Editor 
               | Game

-- MESSAGES

type Message = EditorMessage Editor.Main.Message
               | GameMessage Game.Message.Message
               | SwitchPage
               | Navigate AppPage
               | UrlChange Navigation.Location
