module Editor.Main exposing (..)

import Set

import Common.Annex exposing (..)
import Game.Condition exposing (..)
import Game.Effect exposing (..)
import Game.GameState exposing (GameState)
import Game.Story as Story exposing (Consequence, StoryEvent, Story)
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
  |> (++) (List.map actionsReferencedInEffect (getEffects story))
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


actionsReferencedInEffect : Effect -> List String
actionsReferencedInEffect effect =
  case effect of
    ActivateAction a -> [a]
    DeactivateAction a -> [a]
    Compound2 e1 e2 -> (actionsReferencedInEffect e1) ++ (actionsReferencedInEffect e2)
    Compound effects -> List.map actionsReferencedInEffect effects |> List.concat 
    other -> []


getConditions : Story -> List Condition
getConditions story =
  List.map getConditionsForEvent story
  |> List.concat


getConditionsForEvent : StoryEvent -> List Condition
getConditionsForEvent event =
  [event.trigger] ++ 
  (concatMaybes (List.map .condition event.subsequents))


getEffects : Story -> List Effect
getEffects story =
  List.map getEffectsForEvent story
  |> List.concat


getEffectsForEvent : StoryEvent -> List Effect
getEffectsForEvent event =
  concatMaybes [event.effect] ++ (List.map getEffectsForConsequence event.subsequents |> List.concat )


getEffectsForConsequence : Consequence -> List Effect
getEffectsForConsequence consq =
  case consq.eventOrName of
    Story.EventName s -> []
    Story.ActualEvent e -> getEffectsForEvent e
