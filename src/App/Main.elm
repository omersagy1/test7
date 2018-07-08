import AnimationFrame
import Html.Styled exposing (Html, button, div, text, input)
import Task
import Time

import App.Model exposing (..)
import Render.App

import Editor.Main
import Game.Model
import Game.Update
import Game.Subs


main : Program Never Model Message
main =
  Html.Styled.program
    { init = init
    , view = Render.App.view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Message)
init = (model, Task.perform (GameMessage << Game.Update.StartTime) Time.now)


model : Model
model =
  { currentPage = GamePage
  , editorModel = Editor.Main.initialModel
  , gameModel = Game.Model.initialModel
  }


subscriptions : Model -> Sub Message
subscriptions model =
  case model.currentPage of
    EditorPage -> Sub.none
    GamePage ->
      Sub.map GameMessage (Game.Subs.subscriptions model.gameModel)


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case (msg, model.currentPage) of

    (EditorMessage m, EditorPage) -> 
      ({ model | editorModel = Editor.Main.update m model.editorModel }
      , Cmd.none)

    (GameMessage gameMsg, GamePage) -> 
      ({ model | gameModel = Game.Update.update gameMsg model.gameModel }
      , Cmd.map GameMessage (Game.Update.command gameMsg))

    (SwitchPage, anyPage) ->
      (switchPage model, Cmd.none)

    other ->
      -- Somehow a message was sent for the wrong page.
      -- Drop it and do nothing.
      -- TODO: figure out how to send warnings.
      (model, Cmd.none)


switchPage : Model -> Model
switchPage model =
  if model.currentPage == EditorPage then
    { model | currentPage = GamePage }
  else
    { model | currentPage = EditorPage }