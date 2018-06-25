module Render.Game exposing (view)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Annex exposing(..)

import Game.Cooldown
import Game.Fire exposing (Fire)
import Game.GameState exposing (GameState)
import Game.Model exposing (Model)
import Game.Resource exposing (Resource)
import Game.Update exposing (Message)
import Game.Story exposing (Choice)

import Render.Choice as Choice
import Render.Messages as Messages
import Render.Meter as Meter


view : Model -> Html Message
view m = div [ css [ displayFlex , padding (px 30)] ]
             [ Messages.messageHistory m.messageHistory
             , interactiveDisplay m
             ]



interactiveDisplay : Model -> Html Message
interactiveDisplay m =
  div [ css [flexGrow (num 1)]]
      (concatMaybes
        [ Choice.choiceButtons m
        , fire m.gameState.fire |> Just
        , resourceMeters m.gameState
        ])


fire : Fire -> Html Message
fire f =
  let
    labelText = "stoke fire"
  in
    div [ onClick (Game.Update.StokeFire
                   |> Game.Update.GameplayMessage)
        ]
        [ Meter.meter (Game.Fire.strength f) labelText]


resourceMeters : GameState -> Maybe (Html Message)
resourceMeters s =
  case Game.GameState.activeResources s of
    [] -> Nothing
    resources -> 
      div [] (List.map resourceMeter resources)
      |> Just


resourceMeter : Resource -> Html Message
resourceMeter r =
  let
    labelText = r.name ++ ": " ++ (toString r.amount)
  in
    div [ onClick (Game.Update.HarvestResource r 
                   |> Game.Update.GameplayMessage) 
        ]
        [ Meter.meter (Game.Cooldown.currentFraction r.cooldown)
                      labelText
        ]
