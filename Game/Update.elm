module Game.Update exposing (Message, update)

import Game.Model exposing(Model)


type Message = DummyMessage


update : Message -> Model -> Model
update msg model = model
