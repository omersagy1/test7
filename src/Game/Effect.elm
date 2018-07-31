module Game.Effect exposing (..)

import Game.ActionName as ActionName


type Effect = NoEffect
              | ActivateResource String
              | AddToResource String Int
              | AddToResourceRand String Int Int
              | SubtractResource String Int
              | SetResourceAmount String Int
              | SetMilestoneReached String
              | IncrementMilestone String
              | ActivateAction ActionName.Name
              | DeactivateAction ActionName.Name
              | StokeFire
              | GameOver
              | Compound (List Effect)
              | Compound2 Effect Effect