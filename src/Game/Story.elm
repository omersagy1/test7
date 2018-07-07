module Game.Story exposing(..)

import Common.Randomizer as Randomizer exposing (Randomizer)
import Game.Condition as Condition exposing (Condition)
import Game.ConditionFns as ConditionFns
import Game.Effect as Effect exposing (Effect)
import Game.GameState exposing (GameState)


type alias StoryEvent = 
  { name: String
  , trigger: Condition 
  , text: List Line 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  , effect : Maybe Effect
  , subsequents : ConsequenceSet
  }

type alias Choice =
  { text : String
  , consequenceSet : ConsequenceSet
  } 

type alias ConsequenceSet = List Consequence

type alias Consequence =
  { eventOrName : EventOrName
  , condition : Maybe Condition
  }

type EventOrName = ActualEvent StoryEvent
                   | EventName String

type Line = FixedLine String
            | RandomLines (List String)


eventName : EventOrName -> String
eventName e =
  case e of
    ActualEvent event -> event.name
    EventName name -> name


getText : Line -> Randomizer -> (Maybe String, Randomizer)
getText ln randomizer =
  case ln of
    FixedLine str -> (Just str, randomizer)
    RandomLines strs -> Randomizer.choose strs randomizer


getConsequence : List Consequence -> GameState -> (Maybe EventOrName, GameState)
getConsequence consequences state =
  case consequences of
    [] -> (Nothing, state)
    first::rest ->
      case first.condition of
        Nothing -> (Just first.eventOrName, state)
        Just condition ->
          let
            (success, newState) = ConditionFns.condition condition state
          in
            if success then (Just first.eventOrName, newState)
            else getConsequence rest newState
