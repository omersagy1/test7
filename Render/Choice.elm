module Render.Choice exposing (choiceButtons)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Game.Model exposing (Model)
import Game.Story exposing (Choice)
import Game.Update exposing (Message)


choiceButtons : Model -> Maybe (Html Message)
choiceButtons m =
  case m.activeChoices of
    Nothing -> Nothing
    Just choices ->
      div [] (List.map choiceButton choices) 
      |> Just


choiceButton : Choice -> Html Message
choiceButton c =
  button [ css [ width (px 100)
               , height (px 40)
               , backgroundColor Colors.black
               , color Colors.white
               , borderColor Colors.yellow
               , borderWidth (px 3)
               , borderStyle solid
               , fontSize (px 16)
               ]
         , onClick (Game.Update.MakeChoice c)] 
         [text c.text]
