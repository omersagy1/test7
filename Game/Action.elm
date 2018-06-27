module Game.Action exposing (..)

import Game.Effect exposing (Effect)
import Game.Resource exposing (Resource)


type Action = StokeFire
              | HarvestResource Resource
              | CustomAction (List Effect)