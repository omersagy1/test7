module Render.Game exposing (view)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Common.Annex exposing(..)

import Game.Action exposing (CustomAction)
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
                   , fontFamily serif
                   ] 
             ]
             [ Messages.messageHistory m.messageHistory
             , interactiveDisplay m
             ]



interactiveDisplay : Model -> Html Message
interactiveDisplay m =
  let softPaused = Game.Update.waitingOnChoice m
  in
    div [ css [flexGrow (num 1)]]
        (concatMaybes
          [ Choice.choiceButtons m
          , fire m.gameState softPaused |> Just
          , actionMeters m.gameState softPaused
          , resources m.gameState 
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



resources : GameState -> Maybe (Html Message)
resources s =
  case Game.GameState.activeResources s of
    [] -> Nothing
    rs -> 
      div [] (List.map resource rs)
      |> Just


resource : Resource -> Html a
resource r =
  let
    labelText = r.name ++ ": " ++ (toString r.amount)
  in
    div [css [marginBottom (px 10)]]
        [text labelText]


actionMeters : GameState -> Bool -> Maybe (Html Message)
actionMeters s paused =
  case Game.GameState.activeActions s of
    [] -> Nothing
    actions -> 
      div [] (List.map (actionMeter paused) actions)
      |> Just


actionMeter : Bool -> CustomAction -> Html Message
actionMeter paused a =
  let
    labelText = a.name
  in
    div [ onClick (Game.Action.CA a
                   |> Game.Update.GameplayMessage) 
        ]
        [ Meter.meter (Game.Cooldown.currentFraction a.cooldown)
                      labelText
                      ((Game.Action.canPerform a) && not paused)
        ]
