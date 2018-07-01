module Editor.Main exposing (..)

import Time exposing (Time)
import Queue.TimedQueue as TimedQueue exposing(TimedQueue)

type alias Model = 
  { val: Float
  , corpus: List String
  , display: List String
  , textDraft: String

  , gameTime: Time
  , paused: Bool
  , renderQueue : TimedQueue String
  }

type alias Display = List String


type Message = UpdateTime Time
               | AddText
               | SaveDraft String
               | Play
               | Pause


initialModel : Model
initialModel = 
    { val = 0
    , corpus = []
    , display = []
    , textDraft = ""

    , gameTime = 0
    , paused = True

    , renderQueue = TimedQueue.new
    }


updateEditor : Message -> Model -> Model
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
enqueueMessage q m = TimedQueue.enqueue m (1*Time.second) q


processRenderQueue : TimedQueue String -> Time -> Display -> (TimedQueue String, Display)
processRenderQueue timedQueue timePassed display = 
  let 
    updated = TimedQueue.update timePassed timedQueue
    (m, dequeued) = TimedQueue.dequeue updated
  in
    case m of
      Nothing -> (dequeued, display)
      (Just x) -> (dequeued, display ++ [x])