module Game.Story exposing(..)

import List
import Time exposing(Time)

import Game.GameState as GameState exposing(GameState)


type alias StoryEvent = 
  { name: String
  , trigger: Trigger 
  , text: List String 
  , choices: Maybe (List Choice)
  , occursOnce: Bool
  , mutator : Maybe Mutator
  }


type alias Choice =
  { text : String
  , consequence : Maybe Consequence
  } 


type Consequence = ActualEvent StoryEvent
                   | EventName String


-- TRIGGERS 

type alias Trigger = GameState -> Bool

gameTimePassed : Time -> Trigger
gameTimePassed t = (\s -> s.gameTime >= t)

manualOnly : Trigger
manualOnly s = False

resourceAbove : String -> Int -> Trigger
resourceAbove name amount =
  (\s ->
    case GameState.getResourceNamed name s of
      Nothing -> False
      Just r -> r.amount >= amount)


-- MUTATORS

type alias Mutator = GameState -> GameState

mutateResource : String -> (Int -> Int) -> Mutator
mutateResource = GameState.mutateResource


addToResource : String -> Int -> Mutator
addToResource name x = 
  (\s ->
    let stateWithActiveResource =
      if not (GameState.resourceActive name s) then
        GameState.activateResource name s
      else
        s
    in
      GameState.mutateResource name ((+) x) stateWithActiveResource)


subtractResource : String -> Int -> Mutator
subtractResource name x = GameState.mutateResource name (\y -> y - x)


and : Mutator -> Mutator -> Mutator
and m1 m2 = (\s -> s |> m1 |> m2)
