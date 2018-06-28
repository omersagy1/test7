module Game.Resource exposing (..)


type alias Resource =
  { name : String
  , amount : Int 
  , active : Bool
  }


init : String -> Int -> Resource
init name amount =
  { name = name
  , amount = amount
  , active = False
  }


initialAmount : Int -> Resource -> Resource
initialAmount x r = { r | amount = x }


mutate : (Int -> Int) -> Resource -> Resource
mutate fn r =
  { r | amount = fn r.amount }


add : Int -> Resource -> Resource
add x r = mutate ((+) x) r


subtract : Int -> Resource -> Resource
subtract x r = mutate (\y -> y - x) r


activate : Resource -> Resource
activate r = { r | active = True }


deactivate : Resource -> Resource
deactivate r = { r | active = False }