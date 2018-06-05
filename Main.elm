import AnimationFrame
import Html exposing (Html, button, div, text, input)
import Time exposing (Time, second)

import Annex exposing(zip, enumerate)
import TimedQueue exposing(TimedQueue)

import Model exposing (..)
import Render


main =
  Html.program
    { init = init
    , view = Render.view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Message)
init = (model, Cmd.none)


model : Model
model =
  { currentPage = EditorPage
  , editorModel = 
    { val = 0
    , corpus = []
    , display = []
    , textDraft = ""

    , gameTime = 0
    , paused = True

    , renderQueue = TimedQueue.new
    }
  , gameModel = { dummy = 2 }
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  AnimationFrame.diffs UpdateTime


-- UPDATE

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let newModel = 
    case model.currentPage of
      EditorPage -> { model | editorModel = updateEditor msg model.editorModel }
      GamePage -> { model | gameModel = updateGame msg model.gameModel }
  in
    (newModel, Cmd.none)


updateGame : Message -> GameModel -> GameModel
updateGame msg model = model


updateEditor : Message -> EditorModel -> EditorModel
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
