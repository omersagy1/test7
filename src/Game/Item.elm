module Game.Item exposing (..)


type Item = Item
  { active : Bool
  , name : String
  , amount : Amount
  }


type Amount = Single Bool
              | Countable Int
              | Decaying DecayParams


type alias DecayParams =
  { maxValue : Float
  , decayPerSecond : Float
  }


single : String -> Item
single name = Item
  { active = False
  , name = name
  , amount = Single False
  }


countable : String -> Item
countable name = Item
  { active = False
  , name = name
  , amount = Countable 0
  }


decaying : String -> Float -> Float -> Item
decaying name maxValue decayPerSecond = Item
  { active = False
  , name = name
  , amount = Decaying { maxValue = maxValue, decayPerSecond = decayPerSecond }
  }


activate : Item -> Item
activate (Item i) = Item { i | active = True }


setPossession : Bool -> Item -> Item
setPossession acquired (Item i) =
  case i.amount of
    Single _ -> Item { i | amount = Single acquired }
    other -> Item i


acquire : Item -> Item
acquire = setPossession True


lose : Item -> Item
lose = setPossession False