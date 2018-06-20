module Render.Game exposing (view, navExtension)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)
import Round

import Annex exposing(..)

import Game.Cooldown
import Game.Fire exposing (Fire)
import Game.GameState exposing (GameState, Resource)
import Game.Model exposing (Model)
import Game.Update exposing (Message)
import Game.Story exposing (Choice)

import Render.Meter as Meter

-- TOP BAR NAV --

navExtension : Model -> List (Html Message)
navExtension m = [togglePauseButton m.paused
                 , restartButton]


togglePauseButton : Bool -> Html Message
togglePauseButton paused = 
  let
    buttonText = if paused then "PLAY" else "PAUSE"
  in
    button [ style [("margin", "5px")]
           , onClick Game.Update.TogglePause]
           [text buttonText]


restartButton : Html Message
restartButton = 
  button [ style [("margin", "5px")]
         , onClick Game.Update.Restart]
         [text "RESTART"]


-- MAIN GAME DISPLAY -- 

view : Model -> Html Message
view m = div [ style [("display", "flex")] ]
             [ messageHistory m.messageHistory
             , interactiveDisplay m
             ]

messageHistory : List String -> Html a 
messageHistory msgs =
  div [style [("width", "350px"),
              ("margin-right", "50px")]]
      (List.map message msgs)


message : String -> Html a
message msg = div [] [text msg]


interactiveDisplay : Model -> Html Message
interactiveDisplay m =
  div [style [("flex-grow", "1")]]
      (concatMaybes
        [ choiceButtons m
        , fire m.gameState.fire |> Just
        , resourceMeters m.gameState
        ])


choiceButtons : Model -> Maybe (Html Message)
choiceButtons m =
  case m.activeChoices of
    Nothing -> Nothing
    Just choices ->
      div [] (List.map choiceButton choices) 
      |> Just


choiceButton : Choice -> Html Message
choiceButton c =
  button [onClick (Game.Update.MakeChoice c)] [text c.text]


fire : Fire -> Html Message
fire f =
  let
    labelText = 
      ("stoke fire: " ++ (Game.Fire.strength f |> Round.round 2))
  in
    div [ onClick (Game.Update.StokeFire
                   |> Game.Update.GameplayMessage)
        ]
        [ Meter.meter (Game.Fire.strength f) labelText]


resourceMeters : GameState -> Maybe (Html Message)
resourceMeters s =
  case s.resources of
    [] -> Nothing
    resources -> 
      div [] (List.map resourceMeter resources)
      |> Just


resourceMeter : Resource -> Html Message
resourceMeter r =
  let
    labelText = 
        Game.Cooldown.currentFraction r.cooldown 
        |> Round.round 2
        |> (++) (r.name ++ " (" ++ (toString r.amount) ++ "): ")
  in
    div [ onClick (Game.Update.HarvestResource r 
                   |> Game.Update.GameplayMessage) 
        ]
        [ Meter.meter (Game.Cooldown.currentFraction r.cooldown)
                      labelText
        ]
