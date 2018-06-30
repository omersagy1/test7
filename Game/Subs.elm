module Game.Subs exposing(..)

import AnimationFrame

import Game.Model as Model exposing(Model)
import Game.Update as Update exposing(Message)


subscriptions : Model -> Sub Message
subscriptions model = 
  case model.paused of
    True -> Sub.none
    False -> AnimationFrame.times Update.UpdateTime
