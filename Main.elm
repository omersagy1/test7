import AnimationFrame
import Html exposing (Html, button, div, text, input)
import Time exposing (Time, second)

import Annex exposing(zip, enumerate)
import Queue.TimedQueue as TimedQueue exposing(TimedQueue)

import Model exposing (..)
import Render.App

import Game.Model
import Game.Update
import Game.Subs


main =
  Html.program
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
  , editorModel = 
    { val = 0
    , corpus = []
    , display = []
    , textDraft = ""

    , gameTime = 0
    , paused = True

    , renderQueue = TimedQueue.new
    }
  , gameModel = Game.Model.initialModel
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  case model.currentPage of
    EditorPage ->
      Sub.map EditorMessage (AnimationFrame.diffs UpdateTime)
    GamePage ->
      Sub.map GameMessage (Game.Subs.subscriptions model.gameModel)


-- UPDATE

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let newModel = 
    case msg of

      EditorMessage m -> 
        case model.currentPage of
          EditorPage ->
            { model | editorModel = updateEditor m model.editorModel }
          other ->
            model

      GameMessage m -> 
        case model.currentPage of
          GamePage ->
            { model | gameModel = Game.Update.update m model.gameModel }
          other ->
            model

      SwitchPage ->
        switchPage model
  in
    (newModel, Cmd.none)




updateEditor : EditorMessage -> EditorModel -> EditorModel
updateEditor msg model =
  case msg of

      UpdateTime t -> gameLoop model t

      AddText -> { model | corpus = model.corpus ++ [model.textDraft]
                         , textDraft = "" 
                         , renderQueue = (enqueueMessage model.renderQueue 
                                                         model.textDraft)
                         }

      SaveDraft txt -> { model | textDraft = txt }

      Play -> { model | paused = False }

      Pause -> { model | paused = True }
      

gameLoop : EditorModel -> Time -> EditorModel
gameLoop model timePassed =
  if model.paused then
    model
  else
    let
      (nextRenderQueue, nextDisplay) = 
        processRenderQueue model.renderQueue timePassed model.display
    in
      { model | gameTime = model.gameTime + timePassed
              , renderQueue = nextRenderQueue
              , display = nextDisplay
      }


enqueueMessage : TimedQueue String -> String -> TimedQueue String
enqueueMessage q m = TimedQueue.enqueue m (1*second) q


processRenderQueue : TimedQueue String -> Time -> Display -> (TimedQueue String, Display)
processRenderQueue timedQueue timePassed display = 
  let 
    updated = TimedQueue.update timePassed timedQueue
    (m, dequeued) = TimedQueue.dequeue updated
  in
    case m of
      Nothing -> (dequeued, display)
      (Just x) -> (dequeued, display ++ [x])


switchPage : Model -> Model
switchPage model =
  if model.currentPage == EditorPage then
    { model | currentPage = GamePage }
  else
    { model | currentPage = EditorPage }