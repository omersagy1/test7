module Game.Action exposing (..)

type alias ActionHistory = List Action

type Action = StokeFire

newHistory : ActionHistory
newHistory = []

addAction : Action -> ActionHistory -> ActionHistory
addAction a h = a :: h

clearActions : ActionHistory -> ActionHistory
clearActions h = []

hasAction : Action -> ActionHistory -> Bool
hasAction a h = List.member a h