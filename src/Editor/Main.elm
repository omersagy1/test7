module Editor.Main exposing (..)

import Game.GameState exposing (GameState)
import Parser.Main


type alias Model = 
  { milestonesSet : List String
  , milestonesUsed : List String
  , actionsSet : List String
  , actionsUsed : List String
  }


type Message = Initialize Int


initialModel : Model
initialModel = 
    { milestonesSet = []
    , milestonesUsed = []
    , actionsSet = []
    , actionsUsed = []
    }


update : Message -> Model -> Model
update msg model =
  case msg of
      Initialize x -> analyze model


analyze : Model -> Model
analyze m =
  let
    initialState = Parser.Main.initialGameState
    story = Parser.Main.storyEventCorpus
  in
    { m | actionsSet = actionsSet initialState
    }


actionsSet : GameState -> List String
actionsSet state =
  List.map .name state.customActions
