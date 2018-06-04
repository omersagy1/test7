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
  { val = 0
  , corpus = []
  , display = []
  , textDraft = ""

  , gameTime = 0
  , paused = True

  , renderQueue = TimedQueue.new
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  AnimationFrame.diffs UpdateTime


-- UPDATE



update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let newModel = case msg of

      UpdateTime t -> gameLoop model t

      AddText -> { model | corpus = model.corpus ++ [model.textDraft]
                         , textDraft = "" 
                         , renderQueue = (enqueueMessage model.renderQueue 
                                                         model.textDraft)
                         }

      SaveDraft txt -> { model | textDraft = txt }

      Play -> { model | paused = False }

      Pause -> { model | paused = True }

  in
    (newModel, Cmd.none)
      

gameLoop : Model -> Time -> Model
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



