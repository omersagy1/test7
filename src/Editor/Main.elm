module Editor.Main exposing (..)

import Set

import Common.Annex exposing (..)
import Game.Condition exposing (..)
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
  |> List.sort


actionsUsed : Story -> List String
actionsUsed story = 
  List.map actionsReferenced (getConditions story)
  |> List.concat
  |> Set.fromList
  |> Set.toList


badActionReferences : Model -> List String
badActionReferences model =
  Set.diff (Set.fromList model.actionsUsed)
           (Set.fromList model.actionsSet)
  |> Set.toList


actionsReferenced : Condition -> List String
actionsReferenced cond =
  case cond of
    Not c -> 
      actionsReferenced c
    And c1 c2 -> 
      (actionsReferenced c1) ++ (actionsReferenced c2)
    Or c1 c2 -> 
      (actionsReferenced c1) ++ (actionsReferenced c2)
    Pure (ActionPerformed name) -> 
      [name]
    other -> 
      []


getConditions : Story -> List Condition
getConditions story =
  List.map getConditionsForEvent story
  |> List.concat


getConditionsForEvent : StoryEvent -> List Condition
getConditionsForEvent event =
  [event.trigger] ++ 
  (concatMaybes (List.map .condition event.subsequents))