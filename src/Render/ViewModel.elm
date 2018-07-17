module Render.ViewModel exposing (..)

import Game.Action exposing (CustomAction)
import Game.Cooldown
import Game.Fire
import Game.GameState
import Game.Model exposing (Model)
import Game.StoryEvent exposing (Choice)
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
  , clickable = (Game.GameState.canStokeFire model.gameState) 
                && not (Game.Model.gameplayPaused model)
  , callback = Game.Update.GameplayMessage Game.Action.StokeFire
  }


buildActionMeters : Model -> List Meter
buildActionMeters model =
  List.map (buildActionMeter model) (Game.GameState.activeActions model.gameState)


buildActionMeter : Model -> CustomAction -> Meter
buildActionMeter model action =
  { proportion = Game.Cooldown.currentFraction action.cooldown
  , label = action.name
  , clickable = not (Game.Model.gameplayPaused model) 
                && (Game.Action.canPerform action)
  , callback = Game.Update.GameplayMessage (Game.Action.CA action)
  }


buildChoiceButtons : Model -> List ChoiceButton
buildChoiceButtons model =
  List.map buildChoiceButton model.activeChoices


buildChoiceButton : Choice -> ChoiceButton
buildChoiceButton choice =
  { label = choice.prompt
  , callback = Game.Update.MakeChoice choice
  }