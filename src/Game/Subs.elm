module Game.Subs exposing(..)

import AnimationFrame

import Game.Model as Model exposing (Model)
import Game.Message as Message exposing (Message)


subscriptions : Model -> Sub Message
subscriptions model = 
  if (Model.isGameOver model) 
     || (Model.hardPaused model) then Sub.none
  else AnimationFrame.diffs Message.UpdateTime
