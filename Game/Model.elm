module Game.Model exposing(..)


type alias Model = { dummy: Int }


type Message = DummyMessage


initialModel : Model
initialModel = { dummy = 2 }