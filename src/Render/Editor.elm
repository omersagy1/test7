module Render.Editor exposing (view)

import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (style, value)

import Editor.Main exposing(..)


view : Model -> Html Message
view model = div []
                 [text "hello!"]