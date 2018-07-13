module Render.Choice exposing (choiceButtons)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Game.Model exposing (Model)
import Game.StoryEvent exposing (Choice)
import Game.Update exposing (Message)
import Render.Constants as Constants


choiceButtons : Model -> Maybe (Html Message)
choiceButtons m =
  case m.activeChoices of
    Nothing -> Nothing
    Just choices ->
      div [ css [ marginBottom (px 25) ] ] 
          (List.map choiceButton choices) 
      |> Just


choiceButton : Choice -> Html Message
choiceButton c =
  button [ css [ width (px Constants.choiceButtonLength)
               , height (px Constants.choiceButtonHeight)
               , backgroundColor (rgb 60 60 60)
               , color Colors.white
               , borderColor Colors.yellow
               , borderWidth (px Constants.borderWidth)
               , borderStyle solid
               , fontSize (px 16)
               , marginRight (px Constants.choiceButtonMargin)
               , padding (px 0)
               ]
         , onClick (Game.Update.MakeChoice c)] 
         [text c.prompt]
