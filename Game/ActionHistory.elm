module Game.ActionHistory exposing (..)

import Game.Action as Action exposing (Action)

type alias ActionHistory = List Action

newHistory : ActionHistory
newHistory = []

addAction : Action -> ActionHistory -> ActionHistory
addAction a h = a :: h

clearActions : ActionHistory -> ActionHistory
clearActions h = []

hasAction : Action -> ActionHistory -> Bool
hasAction a h = List.member a h