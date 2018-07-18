module Render.ViewModel exposing (..)

import Game.Action exposing (CustomAction)
import Game.Cooldown
import Game.Fire
import Game.GameState
import Game.Model exposing (Model)
import Game.Printer
import Game.Resource exposing (Resource)
import Game.StoryEvent exposing (Choice)
import Game.Update exposing (Message)


type alias ViewModel =
  { messageHistory : List String
  , fireMeter : Meter
  , actionMeters : List Meter
  , choiceButtons : List ChoiceButton
  , resourceDisplays : List ResourceDisplay
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


type alias ResourceDisplay =
  { name : String
  , amount : Int
  }


init : Model -> ViewModel
init model =
  { messageHistory = Game.Printer.allMessages model
  , fireMeter = buildFireMeter model
  , actionMeters = buildActionMeters model
  , choiceButtons = buildChoiceButtons model
  , resourceDisplays = buildResources model
  }


buildFireMeter : Model -> Meter
buildFireMeter model =
  { proportion = Game.Fire.strength model.gameState.fire
  , label = "stoke fire"
  , clickable = (Game.GameState.canStokeFire model.gameState) 
                && not (Game.Model.softPaused model)
  , callback = Game.Update.GameplayMessage Game.Action.StokeFire
  }


buildActionMeters : Model -> List Meter
buildActionMeters model =
  List.map (buildActionMeter model) (Game.GameState.activeActions model.gameState)


buildActionMeter : Model -> CustomAction -> Meter
buildActionMeter model action =
  { proportion = Game.Cooldown.currentFraction action.cooldown
  , label = action.name
  , clickable = not (Game.Model.softPaused model) 
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


buildResources : Model -> List ResourceDisplay
buildResources model =
  List.map buildResource (Game.GameState.activeResources model.gameState)


buildResource : Resource -> ResourceDisplay
buildResource resource =
  { name = resource.name
  , amount = resource.amount
  }