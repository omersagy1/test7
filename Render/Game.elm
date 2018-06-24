module Render.Game exposing (view, navExtension)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)
import Round

import Annex exposing(..)

import Game.Cooldown
import Game.Fire exposing (Fire)
import Game.GameState exposing (GameState)
import Game.Model exposing (Model)
import Game.Resource exposing (Resource)
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
    button [ css [ margin (px 5) ]
           , onClick Game.Update.TogglePause]
           [text buttonText]


restartButton : Html Message
restartButton = 
  button [ css [ margin (px 5) ]
         , onClick Game.Update.Restart]
         [text "RESTART"]


-- MAIN GAME DISPLAY -- 

view : Model -> Html Message
view m = div [ css [ displayFlex , padding (px 30)] ]
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
    div [css [ width (px 350)
             , marginRight (px 50)
             ]
        ]
        (List.map2 message msgsToDisplay opacities)


message : String -> Float -> Html a
message msg opacityValue = 
  div [ css [ marginBottom (px 10)
            , opacity (num opacityValue)
            ]
      ] 
      [text msg]


interactiveDisplay : Model -> Html Message
interactiveDisplay m =
  div [ css [flexGrow (num 1)]]
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
