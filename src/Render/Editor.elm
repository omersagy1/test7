module Render.Editor exposing (view)

import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (style, value)

import Editor.Main exposing(..)


view : Model -> Html a
view model = 
  div [] 
      [ actionsSet model
      ]

actionsSet : Model -> Html a
actionsSet model = 
  div [] 
      ([ text "The following actions were initialized:" ] ++
       List.map entry model.actionsSet)

entry : String -> Html a
entry s = div [] [text s]