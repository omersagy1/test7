module Render exposing (view)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Annex exposing(zip, enumerate)
import Model exposing(..)


view : Model -> Html Message
view model =
  let mainPage =
    case model.currentPage of
      EditorPage -> Html.map EditorMessage (editorView model.editorModel)
      GamePage -> Html.map GameMessage gameView
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


gameView : Html a
gameView = div [] []

editorView : EditorModel -> Html EditorMessage
editorView model = div []
                 [ corpusHeader
                 , renderCorpus model.corpus
                 , renderAddText model.textDraft
                 , renderPlay
                 , renderPausedStatus model.paused
                 , renderDisplay model.display
                 ]

corpusHeader : Html a
corpusHeader = div [style [("margin", "5px")]]
                   [text "CORPUS"]


renderCorpus : List String -> Html a
renderCorpus l = 
  let 
    numberedLines = List.map (\(x, y) -> toString (x + 1) ++ ". " ++ y)
                             (enumerate l)
    lineDivs = List.map (\x -> div [] [text x]) numberedLines
  in
    div [style [("margin", "5px")]] lineDivs


renderAddText : String -> Html EditorMessage
renderAddText draft = 
  div [ style[("margin", "5px")] ]
      [ input [ onInput SaveDraft
                , value draft
              ] 
              []
      , button [onClick AddText] [text "Add Text"]
      ]


renderPlay : Html EditorMessage
renderPlay = div [style[("margin", "5px")]] 
                 [button [style [("margin-right", "5px")]
                         ,onClick Play] 
                         [text "PLAY"]
                 ,button [onClick Pause] [text "PAUSE"]]

renderPausedStatus : Bool -> Html EditorMessage
renderPausedStatus paused =
  let msg = if paused then "PAUSED" else "PLAYING"
  in
    div [style [("margin", "5px")]] 
        [text msg]

renderDisplay : Display -> Html EditorMessage
renderDisplay messages = 
  let 
    numberedLines = List.map (\(x, y) -> toString (x + 1) ++ ". " ++ y)
                             (enumerate messages)
    lineDivs = List.map (\x -> div [] [text x]) numberedLines
  in
    div [style [("margin", "5px")]] lineDivs
