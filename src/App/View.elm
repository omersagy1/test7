module App.View exposing (view)

import Css exposing (..)
import Css.Foreign
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

      EditorPage -> Html.Styled.map EditorMessage 
                      (Editor.View.view model.editorModel)

      GamePage -> Html.Styled.map GameMessage 
                    (Render.View.view model.gameModel)
  in
    styled div 
      [ height (pct 100) 
      , fontFamily serif
      ] 
      [] 
      [ globalStyle
      , navBar model 
      , mainPage 
      ]


globalStyle : Html a
globalStyle =
  Css.Foreign.global
    [ Css.Foreign.selector "html"
      [ height (pct 100) 
      ]
    , Css.Foreign.selector "body"
      [ height (pct 100) 
      , margin (px 0)
      ]
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
    GamePage ->
      (List.map (\h -> Html.Styled.map GameMessage h) 
                (App.NavBar.navExtension m.gameModel))
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
