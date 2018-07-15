module Game.Subs exposing(..)

import AnimationFrame

import Game.Model as Model exposing(Model)
import Game.Update as Update exposing(Message)


subscriptions : Model -> Sub Message
subscriptions model = 
  if model.gameState.gameOver then Sub.none
  else if model.paused then Sub.none
  else AnimationFrame.diffs Update.UpdateTime
