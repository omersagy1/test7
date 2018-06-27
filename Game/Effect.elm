module Game.Effect exposing (..)


type Effect = ActivateResource String
              | AddToResource String Int
              | SubtractResource String Int
              | SetResourceAmount String Int
              | SetMilestoneReached String
              | Compound (List Effect)