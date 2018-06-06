module RenderGame exposing (gameView)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)


gameView : Html a
gameView = div [] [text "game!"]
