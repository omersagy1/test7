module Render.Editor exposing (editorView)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)

import Annex exposing(zip, enumerate)
import Model exposing(..)


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
