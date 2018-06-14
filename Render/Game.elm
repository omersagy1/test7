module Render.Game exposing (gameView)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Game.Model exposing (Model)
import Game.Update exposing (Message)


gameView : Model -> Html Message
gameView m = div [] [togglePauseButton m]


togglePauseButton : Model -> Html Message
togglePauseButton m = 
  let
    buttonText = if m.paused then "PLAY" else "PAUSE"
  in
    div [style[("margin", "5px")]] 
        [button [onClick Game.Update.TogglePause]
                [text buttonText]]
