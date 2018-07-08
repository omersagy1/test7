module Editor.Main exposing (..)

import Parser.Main


type alias Model = 
  { milestonesSet : List String
  , milestonesUsed : List String
  }


type Message = AnalyzeStory


initialModel : Model
initialModel = 
    { milestonesSet = []
    , milestonesUsed = []
    }


update : Message -> Model -> Model
update msg model =
  case msg of
      AnalyzeStory -> analyze model


analyze : Model -> Model
analyze m = m