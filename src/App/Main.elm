import Html.Styled exposing (Html)
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
init = 
  case model.currentPage of
    EditorPage -> initializeEditor model
    GamePage -> initializeGame model


model : Model
model =
  { currentPage = EditorPage
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
      switchPage model

    other ->
      -- Somehow a message was sent for the wrong page.
      -- Drop it and do nothing.
      -- TODO: figure out how to send warnings.
      (model, Cmd.none)


switchPage : Model -> (Model, Cmd Message)
switchPage model =
  if model.currentPage == EditorPage then
    ({ model | currentPage = GamePage }, Cmd.none)
  else
    initializeEditor { model | currentPage = EditorPage }


initializeGame : Model -> (Model, Cmd Message)
initializeGame m = (m, Task.perform (GameMessage << Game.Update.StartTime) Time.now)


initializeEditor : Model -> (Model, Cmd Message)
initializeEditor = update (EditorMessage Editor.Main.Initialize)