module Render.App exposing (view)

import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (style, value)

import Model exposing(..)

import Render.Game
import Render.Editor
import Render.NavBar


view : Model -> Html Message
view model =
  let mainPage =
    case model.currentPage of

      EditorPage -> Html.Styled.map EditorMessage 
                      (Render.Editor.editorView model.editorModel)

      GamePage -> Html.Styled.map GameMessage 
                    (Render.Game.view model.gameModel)
  in
    div [style [("height", "100%")]] 
        [ navBar model 
        , mainPage ]

navBar : Model -> Html Message
navBar model = div [ style [ ("background-color", "grey")
                           , ("text-color", "white")
                           ] 
                   ] 
                   -- ([switchPageButton model.currentPage] 
                   (navExtension model)


navExtension : Model -> List (Html Message)
navExtension m =
  case m.currentPage of
    GamePage ->
      (List.map (\h -> Html.Styled.map GameMessage h) 
                (Render.NavBar.navExtension m.gameModel))
    other ->
      []


switchPageButton : AppPage -> Html Message
switchPageButton currentPage = 
  let 
    buttonText = 
      case currentPage of
        EditorPage -> "START GAME"
        GamePage -> "GO TO EDITOR"
  in    
    button [ style [("margin", "5px")]
           , onClick SwitchPage
           ] 
           [ text buttonText ]
