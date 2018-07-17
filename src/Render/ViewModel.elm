module Render.ViewModel exposing (..)

import Game.Action
import Game.Fire
import Game.Model exposing (Model)
import Game.GameState
import Game.Update exposing (Message)


type alias ViewModel =
  { messageHistory : List String
  , fireMeter : Meter
  , actionMeters : List Meter
  , choiceButtons : List ChoiceButton
  }


type alias Meter =
  { proportion : Float
  , label : String
  , clickable : Bool
  , callback : Message
  }


type alias ChoiceButton =
  { label : String
  , callback : Message
  }


viewModel : Model -> ViewModel
viewModel model =
  { messageHistory = model.messageHistory
  , fireMeter = buildFireMeter model
  , actionMeters = buildActionMeters model
  , choiceButtons = buildChoiceButtons model
  }


buildFireMeter : Model -> Meter
buildFireMeter model =
  { proportion = Game.Fire.strength model.gameState.fire
  , label = "stoke fire"
  , clickable = Game.GameState.canStokeFire model.gameState
  , callback = Game.Update.GameplayMessage Game.Action.StokeFire
  }


fireClickable : Model -> Bool
fireClickable model =
  (Game.GameState.canStokeFire model.gameState) 
  && not (Game.Model.gameplayPaused model)


buildActionMeters : Model -> List Meter
buildActionMeters model = []


buildChoiceButtons : Model -> List ChoiceButton
buildChoiceButtons model = []