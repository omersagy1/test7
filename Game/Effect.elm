module Game.Effect exposing (..)


type Effect = NoEffect
              | ActivateResource String
              | AddToResource String Int
              | SubtractResource String Int
              | SetResourceAmount String Int
              | SetMilestoneReached String
              | ActivateAction String
              | DeactivateAction String
              | Compound (List Effect)
              | Compound2 Effect Effect