module Render.Choice exposing (choiceButtons)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Game.Message exposing (Message)
import Render.Constants as Constants
import Render.ViewModel exposing (ChoiceButton)


choiceButtons : List (ChoiceButton) -> Maybe (Html Message)
choiceButtons choiceButtons =
  case choiceButtons of
    [] -> Nothing
    choices ->
      div [ css [ marginBottom (px 25) ] ] 
          (List.map choiceButton choices) 
      |> Just


choiceButton : ChoiceButton -> Html Message
choiceButton c =
  div [ css [ width (px Constants.choiceButtonLength)
            , height (px Constants.choiceButtonHeight)
            , backgroundColor (rgb 60 60 60)
            , color Colors.white
            , borderColor Colors.yellow
            , borderWidth (px Constants.borderWidth)
            , borderStyle solid
            , marginBottom (px Constants.choiceButtonMargin)
            , padding (px 0)
            , displayFlex
            , alignItems center
            , justifyContent center
            ]
         , onClick c.callback
         ] 
         [text c.label]
