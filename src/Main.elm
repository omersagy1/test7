import AnimationFrame
import Html.Styled exposing (Html, button, div, text, input)

import Model exposing (..)
import Render.App

import Editor.Main
import Game.Model
import Game.Update
import Game.Subs


main =
  Html.Styled.program
    { init = init
    , view = Render.App.view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Message)
init = (model, Cmd.none)


model : Model
model =
  { currentPage = GamePage
  , editorModel = Editor.Main.initialModel
  , gameModel = Game.Model.initialModel
  }


subscriptions : Model -> Sub Message
subscriptions model =
  case model.currentPage of
    EditorPage ->
      Sub.map EditorMessage (AnimationFrame.diffs Editor.Main.UpdateTime)
    GamePage ->
      Sub.map GameMessage (Game.Subs.subscriptions model.gameModel)


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case (msg, model.currentPage) of

    (EditorMessage m, EditorPage) -> 
      ({ model | editorModel = Editor.Main.updateEditor m model.editorModel }
      , Cmd.none)

    (GameMessage m, GamePage) -> 
      ({ model | gameModel = Game.Update.update m model.gameModel }
      , Cmd.none)

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