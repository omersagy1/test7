module Render.Game exposing (view, navExtension)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Game.Model exposing (Model)
import Game.Update exposing (Message)


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


interactiveDisplay : Model -> Html a
interactiveDisplay m = text "second column"
