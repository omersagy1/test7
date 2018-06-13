module Game.Event exposing(..)


type Event = DisplayText String
             | DisplayChoice List String 
             | TriggerStoryEvent StoryEvent
