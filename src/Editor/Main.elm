module Editor.Main exposing (..)

import Set

import Common.Annex exposing (concatMaybes)
import Game.Condition exposing (..)
import Game.Effect exposing (..)
import Game.GameState exposing (GameState)
import Game.Story as Story exposing (Story)
import Game.StoryEvent as StoryEvent exposing (..)
import Parser.Main


type alias Model = 
  { milestonesSet : List String
  , milestonesUsed : List String
  , actionsSet : List String
  , actionsUsed : List String
  , eventNames : List String
  , eventsReferenced : List String
  , resourcesSet : List String
  , resourcesUsed : List String
  }


type Message = Initialize


initialModel : Model
initialModel = 
    { milestonesSet = []
    , milestonesUsed = []
    , actionsSet = []
    , actionsUsed = []
    , eventNames = []
    , eventsReferenced = []
    , resourcesSet = []
    , resourcesUsed = []
    }


update : Message -> Model -> Model
update msg model =
  case msg of
      Initialize -> analyze model


analyze : Model -> Model
analyze m =
  let
    initialState = Parser.Main.initialGameState
    story = Parser.Main.story
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
  List.map actionsReferencedInCondition (getAllConditions story)
  |> (++) (List.map actionsReferencedInEffect (getAllEffects story))
  |> List.concat
  |> Set.fromList
  |> Set.toList


badActionReferences : Model -> List String
badActionReferences model =
  Set.diff (Set.fromList model.actionsUsed)
           (Set.fromList model.actionsSet)
  |> Set.toList


unusedActions : Model -> List String
unusedActions model = 
  Set.diff (Set.fromList model.actionsSet)
           (Set.fromList model.actionsUsed)
  |> Set.toList


actionsReferencedInCondition : Condition -> List String
actionsReferencedInCondition cond =
  case cond of
    Not c -> 
      actionsReferencedInCondition c
    And c1 c2 -> 
      (actionsReferencedInCondition c1) ++ (actionsReferencedInCondition c2)
    Or c1 c2 -> 
      (actionsReferencedInCondition c1) ++ (actionsReferencedInCondition c2)
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


getAllConditions : Story -> List Condition
getAllConditions story =
  List.map StoryEvent.getTrigger story
  |> (++) (List.concat (List.map getConditions (List.map StoryEvent.getEvent story)))


getConditions : StoryEvent -> List Condition
getConditions event =
  List.concat (StoryEvent.map (conditionMatcher) event)


-- Just matches a node without recursing.
conditionMatcher : StoryEvent -> List Condition
conditionMatcher event =
  case event of
    Conditioned (ConditionedEvent condition e) -> [condition]
    PlayerChoice choices -> List.map .condition choices
    other -> []


getAllEffects : Story -> List Effect
getAllEffects story =
  List.concat (List.map getEffects (List.map StoryEvent.getEvent story))


getEffects : StoryEvent -> List Effect
getEffects event = concatMaybes (StoryEvent.map effectMatcher event)


-- Just matches a node without recursing.
effectMatcher : StoryEvent -> Maybe Effect
effectMatcher event =
  case event of
    Atomic (Effectful eff) -> Just eff
    other -> Nothing