module Render.Game exposing (gameView)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Game.Model exposing (Model)
import Game.Update exposing (Message)


gameView : Model -> Html Message
gameView m = div [] 
  [ togglePauseButton m.paused
  , messageHistory m.messageHistory
  ]


togglePauseButton : Bool -> Html Message
togglePauseButton paused = 
  let
    buttonText = if paused then "PLAY" else "PAUSE"
  in
    div [style[("margin", "5px")]] 
        [button [onClick Game.Update.TogglePause]
                [text buttonText]]

messageHistory : List String -> Html a 
messageHistory msgs =
  div [] (List.map message msgs)

message : String -> Html a
message msg = div [] [text msg]