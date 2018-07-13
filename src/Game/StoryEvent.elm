module Game.StoryEvent exposing (..)

import Game.Condition as Condition exposing (Condition)
import Game.Effect as Effect exposing (Effect)


type TopLevelEvent = 
  TopLevel { name : String
           , reoccurring : Bool
           , trigger : Condition
           , event : StoryEvent
           }


type StoryEvent = Atomic AtomicEvent
                  | Compound CompoundEvent


type AtomicEvent =
  Empty
  -- Displays a line of text as-is.
  | Narration String
  -- Dialogue causes "interaction mode" to start if it hasn't started already.
  -- This freezes many aspects of the game. It will also print the text in quotes.
  | Dialogue String
  -- Ends interaction mode and resumes the running of time in the game.
  | EndInteraction
  -- Reference to the name of another StoryEvent.
  | Goto String
  -- Executes some kind of state-mutating effect on the game.
  | Effectful Effect


type CompoundEvent =
  -- Plays one StoryEvent after another.
  Sequenced StoryEvent StoryEvent
  -- A list of possible choices for the player, with the text prompt for each.
  -- Choices will only appear if their condition is either Nothing or evaluates to True.
  | PlayerChoice (List Choice)
  -- A storyevent that only runs if its condition evaluates to True.
  | Conditioned Condition StoryEvent
  -- All options have equal weight.
  | Random (List StoryEvent)
  -- Allows assigning a custom weight to each option.
  | RandomWeighted (List (Float, StoryEvent))
  -- First event which satisfies its condition is picked.
  | Ranked (List StoryEvent) 


type alias Choice =
  { cond: Maybe Condition
  , prompt: String
  , consq: StoryEvent 
  }

topLevel : TopLevelEvent
topLevel = TopLevel
  { name = ""
  , reoccurring = False
  , trigger = Condition.Pure Condition.Never
  , event = Atomic Empty
  }

getName : TopLevelEvent -> String
getName (TopLevel e) = e.name

getTrigger : TopLevelEvent -> Condition
getTrigger (TopLevel e) = e.trigger

getEvent : TopLevelEvent -> StoryEvent
getEvent (TopLevel e) = e.event

isReoccurring : TopLevelEvent -> Bool
isReoccurring (TopLevel e) = e.reoccurring

name : String -> TopLevelEvent -> TopLevelEvent
name s (TopLevel p) = TopLevel { p | name = s }

reoccurring : TopLevelEvent -> TopLevelEvent
reoccurring (TopLevel p) = TopLevel { p | reoccurring = True }

trigger : Condition -> TopLevelEvent -> TopLevelEvent
trigger t (TopLevel p) = TopLevel { p | trigger = t }

body : StoryEvent -> TopLevelEvent -> TopLevelEvent
body e (TopLevel p) = TopLevel { p | event = e }

start : StoryEvent
start = Atomic Empty

seq : StoryEvent -> StoryEvent -> StoryEvent
seq e1 e2 = Compound <| Sequenced e2 e1

ln : String -> StoryEvent -> StoryEvent
ln s e = seq (Atomic <| Narration s) e

effect : Effect -> StoryEvent -> StoryEvent
effect eff e = seq (Atomic <| Effectful eff) e 