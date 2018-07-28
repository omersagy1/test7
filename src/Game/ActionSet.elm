module Game.ActionSet exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Game.Action as Action exposing (Action)


type alias ActionSet = Dict String Action


init : ActionSet
init = Dict.empty


addAction : Action -> ActionSet -> ActionSet
addAction a s = Dict.insert (key a.name) a s


clearActions : ActionSet
clearActions = init


hasAction : Action -> ActionSet -> Bool
hasAction a s = hasActionNamed a.name s


hasActionNamed : Action.Name -> ActionSet -> Bool
hasActionNamed n s = Dict.member (key n) s


getAction : Action.Name -> ActionSet -> Maybe Action
getAction n = Dict.get (key n)


activeActions : ActionSet -> List Action
activeActions s = Dict.filter (\_ a -> a.active) s
                  |> Dict.values


update : Time -> ActionSet -> ActionSet
update t s = Dict.map (\_ -> Action.updateCooldown t) s


map : (Action -> Action) -> ActionSet -> ActionSet
map f s = Dict.map (\_ -> f) s


applyIfNamed : Action.Name -> (Action -> Action) -> Action -> Action
applyIfNamed name f a = if name == a.name then f a else a


applyToNamed : Action.Name -> (Action -> Action) -> ActionSet -> ActionSet
applyToNamed name f s = map (applyIfNamed name f) s


names : ActionSet -> List String
names s = Dict.keys s


key : Action.Name -> String
key n = Action.nameAsString n