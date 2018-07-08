module Editor.Main exposing (..)

import Common.Annex exposing (..)
import Game.Condition exposing (Condition)
import Game.GameState exposing (GameState)
import Game.Story exposing (StoryEvent, Story)
import Parser.Main


type alias Model = 
  { milestonesSet : List String
  , milestonesUsed : List String
  , actionsSet : List String
  , actionsUsed : List String
  }


type Message = Initialize


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
      Initialize -> analyze model


analyze : Model -> Model
analyze m =
  let
    initialState = Parser.Main.initialGameState
    story = Parser.Main.storyEventCorpus
  in
    { m | actionsSet = actionsSet initialState
        , actionsUsed = actionsUsed story
    }


actionsSet : GameState -> List String
actionsSet state =
  List.map .name state.customActions


actionsUsed : Story -> List String
actionsUsed events = []

-- TODO: need to filter conditions by whether they contain
-- action subconditions, then extract the names.


filterConditions : (Condition -> Bool) -> Story -> List Condition
filterConditions pred story =
  List.map (filterConditionsForEvent pred) story
  |> List.concat


filterConditionsForEvent : (Condition -> Bool) -> StoryEvent -> List Condition
filterConditionsForEvent pred event =
  let
    unfiltered = [event.trigger] ++ 
                 (concatMaybes (List.map .condition event.subsequents))
  in
    List.filter pred unfiltered