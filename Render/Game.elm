module Render.Game exposing (view, navExtension)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Game.Model exposing (Model)
import Game.Update exposing (Message)


view : Model -> Html Message
view m = div [] [messageHistory m.messageHistory]


navExtension : Model -> List (Html Message)
navExtension m = [togglePauseButton m.paused]


togglePauseButton : Bool -> Html Message
togglePauseButton paused = 
  let
    buttonText = if paused then "PLAY" else "PAUSE"
  in
    button [ style [("margin", "5px")]
           , onClick Game.Update.TogglePause]
           [text buttonText]

messageHistory : List String -> Html a 
messageHistory msgs =
  div [] (List.map message msgs)

message : String -> Html a
message msg = div [] [text msg]