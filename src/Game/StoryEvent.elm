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
  -- First event which satisfies its condition is picked.
  | Ranked (List StoryEvent) 


type alias Choice =
  { condition: Condition
  , prompt: String
  , consq: StoryEvent 
  }


getName : TopLevelEvent -> String
getName (TopLevel e) = e.name

getTrigger : TopLevelEvent -> Condition
getTrigger (TopLevel e) = e.trigger

getEvent : TopLevelEvent -> StoryEvent
getEvent (TopLevel e) = e.event

isReoccurring : TopLevelEvent -> Bool
isReoccurring (TopLevel e) = e.reoccurring


-- Append a StoryEvent to a top-level chain of StoryEvents.
-- Will append to the first non-sequenced event found.
append : StoryEvent -> StoryEvent -> StoryEvent
append event latest =
  case event of
    Compound (Sequenced prev next) -> Compound (Sequenced prev (append next latest))
    other -> Compound (Sequenced other latest)