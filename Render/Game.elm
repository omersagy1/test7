module Render.Game exposing (view)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Annex exposing(..)

import Game.Action
import Game.Cooldown
import Game.Fire exposing (Fire)
import Game.GameState exposing (GameState)
import Game.Model exposing (Model)
import Game.Resource exposing (Resource)
import Game.Update exposing (Message)

import Render.Choice as Choice
import Render.Messages as Messages
import Render.Meter as Meter


view : Model -> Html Message
view m = div [ css [ displayFlex 
                   , height (pct 100)
                   , padding (px 30)
                   , backgroundColor Colors.black
                   , color Colors.white
                   ] 
             ]
             [ Messages.messageHistory m.messageHistory
             , interactiveDisplay m
             ]



interactiveDisplay : Model -> Html Message
interactiveDisplay m =
  let paused = Game.Update.gameplayPaused m
  in
    div [ css [flexGrow (num 1)]]
        (concatMaybes
          [ Choice.choiceButtons m
          , fire m.gameState paused |> Just
          , resourceMeters m.gameState paused
          ])


fire : GameState -> Bool -> Html Message
fire s paused =
  let
    f = s.fire
    labelText = "stoke fire"
  in
    div [ onClick (Game.Action.StokeFire
                   |> Game.Update.GameplayMessage)
        ]
        [ Meter.meter (Game.Fire.strength f) 
                      labelText 
                      ((Game.GameState.canStokeFire s) && not paused)
        ]



resourceMeters : GameState -> Bool -> Maybe (Html Message)
resourceMeters s paused =
  case Game.GameState.activeResources s of
    [] -> Nothing
    resources -> 
      div [] (List.map (resourceMeter paused) resources)
      |> Just


resourceMeter : Bool -> Resource -> Html Message
resourceMeter paused r =
  let
    labelText = r.name ++ ": " ++ (toString r.amount)
  in
    div [ onClick (Game.Action.HarvestResource r 
                   |> Game.Update.GameplayMessage) 
        ]
        [ Meter.meter (Game.Cooldown.currentFraction r.cooldown)
                      labelText
                      ((Game.Resource.canHarvest r) && not paused)
        ]
