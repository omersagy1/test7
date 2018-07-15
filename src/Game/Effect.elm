module Game.Effect exposing (..)


type Effect = NoEffect
              | ActivateResource String
              | AddToResource String Int
              | AddToResourceRand String Int Int
              | SubtractResource String Int
              | SetResourceAmount String Int
              | SetMilestoneReached String
              | IncrementMilestone String
              | ActivateAction String
              | DeactivateAction String
              | GameOver
              | Compound (List Effect)
              | Compound2 Effect Effect
