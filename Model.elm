module Model exposing(..)

import Time exposing (Time)
import TimedQueue exposing(TimedQueue)


type alias Model = 
  { val: Float
  , corpus: List String
  , display: List String
  , textDraft: String

  , gameTime: Time
  , paused: Bool
  , renderQueue : TimedQueue.TimedQueue String
  }


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


type alias Display = List String


type Message = UpdateTime Time
               | AddText
               | SaveDraft String
               | Play
               | Pause
