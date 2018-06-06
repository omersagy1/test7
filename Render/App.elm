module Render.App exposing (view)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Annex exposing(zip, enumerate)
import Model exposing(..)

import Render.Game
import Render.Editor


view : Model -> Html Message
view model =
  let mainPage =
    case model.currentPage of

      EditorPage -> Html.map EditorMessage 
                      (Render.Editor.editorView model.editorModel)

      GamePage -> Html.map GameMessage Render.Game.gameView
  in
    div [] [ navBar model
           , mainPage
           ]

navBar : Model -> Html Message
navBar model = div [ style [ ("margin-bottom", "15px")
                           , ("background-color", "blue")
                           , ("text-color", "white")
                           ] 
                   ] 
                   [switchPageButton model.currentPage]

switchPageButton : AppPage -> Html Message
switchPageButton currentPage = 
  let 
    buttonText = 
      case currentPage of
        EditorPage -> "START GAME"
        GamePage -> "GO TO EDITOR"
  in    
    button [ style [("margin", "5px")]
             , onClick SwitchPage] 
           [ text buttonText ]


