module App.NavBar exposing (navExtension)

import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Css exposing (..)
import Game.Update exposing (Message)
import Game.Model exposing (Model)


navExtension : Model -> List (Html Message)
navExtension m = [ restartButton
                 , toggleFastForwardButton m.fastForward
                 , togglePauseButton m.paused
                 ]


togglePauseButton : Bool -> Html Message
togglePauseButton paused = 
  let
    buttonText = if paused then "PLAY" else "PAUSE"
  in
    button [ css [ margin (px 5) ]
           , onClick Game.Update.TogglePause]
           [text buttonText]


restartButton : Html Message
restartButton = 
  button [ css [ margin (px 5) ]
         , onClick Game.Update.Restart]
         [text "RESTART"]


toggleFastForwardButton : Bool -> Html Message
toggleFastForwardButton fastForward =
  let
    buttonText = if fastForward then "RESTORE SPEED" else "SPEED UP"
  in
    button [ css [ margin (px 5)]
           , onClick Game.Update.ToggleFastForward
           ]
           [text buttonText]