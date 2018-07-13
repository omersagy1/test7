module Game.Story exposing (..)

import Game.ConditionFns as ConditionFns
import Game.GameState as GameState exposing (GameState)
import Game.StoryEvent as StoryEvent exposing (TopLevelEvent, StoryEvent)


type alias Story = List TopLevelEvent

begin : Story
begin = []

add : TopLevelEvent -> Story -> Story
add e s = s ++ [e]


getEventByName : String -> Story -> Maybe TopLevelEvent
getEventByName name story =
  let
    matches = List.filter (\e -> StoryEvent.getName e == name) story
  in
    case matches of
      [] -> Nothing
      [e] -> Just e
      -- Only return the first match.
      e::others -> Just e
    

-- Returns (triggered events, remaining story, gamestate)
triggeredStoryEvents : GameState -> Story -> (List StoryEvent, Story, GameState)
triggeredStoryEvents state story = triggeredHelper state story [] []

triggeredHelper : GameState -> Story -> List StoryEvent -> Story -> (List StoryEvent, Story, GameState)
triggeredHelper state toscan triggered remaining =
  case toscan of
    [] -> (triggered, remaining, state)
    first::rest ->
      let
        (eventTriggered, newState) = 
          (ConditionFns.condition(StoryEvent.getTrigger first) state)
        triggered2 = if eventTriggered then 
                       (StoryEvent.getEvent first)::triggered 
                     else triggered
        shouldRemove = eventTriggered && not (StoryEvent.isReoccurring first)
        remaining2 = if shouldRemove then remaining else first::remaining
      in
        triggeredHelper newState rest triggered2 remaining2