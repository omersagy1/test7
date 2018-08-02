module App.View exposing (view)

import Css exposing (..)
import Html.Styled exposing (Html, button, div, text, input, styled)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (style, value, css)

import App.Model exposing (..)
import App.NavBar
import Editor.View
import Render.View


view : Model -> Html Message
view model =
  let mainPage =
    case model.currentPage of

      Editor -> Html.Styled.map EditorMessage 
                  (Editor.View.view model.editorModel)

      Game -> Html.Styled.map GameMessage 
               (Render.View.view model.gameModel)
  in
    styled div 
      [ height (pct 100) 
      , fontFamily serif
      ] 
      [] 
      [ navBar model 
      , mainPage 
      ]


navBar : Model -> Html Message
navBar model = div [ style [ ("background-color", "grey")
                           , ("text-color", "white")
                           ] 
                   ] 
                   ([switchPageButton model.currentPage] ++
                    (navExtension model))


navExtension : Model -> List (Html Message)
navExtension m =
  case m.currentPage of
    Game ->
      (List.map (\h -> Html.Styled.map GameMessage h) 
                (App.NavBar.navExtension m.gameModel))
    other ->
      []


switchPageButton : AppPage -> Html Message
switchPageButton currentPage = 
  let 
    buttonText = 
      case currentPage of
        Editor -> "START GAME"
        Game -> "GO TO EDITOR"
  in    
    button [ style [("margin", "5px")]
           , onClick SwitchPage
           ] 
           [ text buttonText ]
