module Parser.Effect exposing (..)

import Game.ActionName as ActionName
import Game.Effect as Effect exposing (..)


activateAction = ActionName.UserDefined >> ActivateAction