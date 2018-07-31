module Game.ActionSet exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

import Game.Action as Action exposing (Action)
import Game.ActionName as ActionName exposing (Name)


type alias ActionSet = Dict String Action


init : ActionSet
init = Dict.empty


addAction : Action -> ActionSet -> ActionSet
addAction a s = Dict.insert (key a.name) a s


clearActions : ActionSet
clearActions = init


hasAction : Action -> ActionSet -> Bool
hasAction a s = hasActionNamed a.name s


hasActionNamed : Name -> ActionSet -> Bool
hasActionNamed n s = Dict.member (key n) s


getAction : Name -> ActionSet -> Maybe Action
getAction n = Dict.get (key n)


activeActions : ActionSet -> List Action
activeActions s = Dict.filter (\_ a -> a.active) s
                  |> Dict.values


update : Time -> ActionSet -> ActionSet
update t s = Dict.map (\_ -> Action.updateCooldown t) s


map : (Action -> Action) -> ActionSet -> ActionSet
map f s = Dict.map (\_ -> f) s


filter : (Action -> Bool) -> ActionSet -> ActionSet
filter pred s = Dict.filter (\_ -> pred) s


applyIfNamed : Name -> (Action -> Action) -> Action -> Action
applyIfNamed name f a = if name == a.name then f a else a


applyToNamed : Name -> (Action -> Action) -> ActionSet -> ActionSet
applyToNamed name f s = map (applyIfNamed name f) s


names : ActionSet -> List String
names s = Dict.keys s


-- user-defined names only.
userDefinedNames : ActionSet -> List String
userDefinedNames s = filter (\a -> case a.name of 
                                    ActionName.UserDefined x -> True
                                    other -> False)
                            s
                     |> names


key : Name -> String
key n = Action.nameAsString n