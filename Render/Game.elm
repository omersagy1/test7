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
navExtension m = [ restartButton
                 , togglePauseButton m.paused
                 ]


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
view m = div [ style [("display", "flex"), ("padding", "30px")] ]
             [ messageHistory m.messageHistory
             , interactiveDisplay m
             ]

messageHistory : List String -> Html a 
messageHistory msgs =
  let 
    displaySize = 8
    msgsToDisplay = List.take displaySize msgs
    opacities = rangeToZero 1 displaySize
  in
    div [style [("width", "350px"),
                ("margin-right", "50px")]]
        (List.map2 message msgsToDisplay opacities)


message : String -> Float -> Html a
message msg opacity = 
  div [style [("margin-bottom", "10px"),
              ("opacity", opacity |> toString)]
      ] 
      [text msg]


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
  button [ style [("width", "100px"),
                  ("height", "40px"),
                  ("background", "dimgray"),
                  ("color", "white"),
                  ("borderColor", "yellow"),
                  ("borderWidth", "3px"),
                  ("borderStyle", "solid"),
                  ("fontSize", "16")
                  ]
         , onClick (Game.Update.MakeChoice c)] 
         [text c.text]


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
  case s.resources of
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
