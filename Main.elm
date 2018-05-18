import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style)
import AnimationFrame
import Time exposing (Time, second)

import Annex exposing(zip, enumerate)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Message)
init = (model, Cmd.none)


-- MODEL

type alias Model = 
  { val: Float
  , corpus: List String
  , display: List String
  , textDraft: String
  }

model : Model
model = 
  { val = 0
  , corpus = []
  , display = []
  , textDraft = ""
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  AnimationFrame.diffs UpdateTime


-- UPDATE

type Message = UpdateTime Time
               | AddText
               | SaveDraft String
               | Run
               | Pause


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let newModel = case msg of

      UpdateTime t -> model

      AddText -> { model | corpus = model.corpus ++ [model.textDraft] }

      SaveDraft t -> { model | textDraft = t }

      Run -> model

      Pause -> model

  in
    (newModel, Cmd.none)
      


-- VIEW

view : Model -> Html Message
view model = div []
                 [corpusHeader, renderCorpus model.corpus, renderAddText]


corpusHeader : Html a
corpusHeader = div [style [("margin-bottom", "10px")]]
                   [text "CORPUS"]


renderCorpus : List String -> Html a
renderCorpus l = 
  let 
    numberedLines = List.map (\(x, y) -> toString (x + 1) ++ ". " ++ y)
                             (enumerate l)
    lineDivs = renderCorpusHelper numberedLines
  in
    div [] lineDivs


renderCorpusHelper : List String -> List (Html a)
renderCorpusHelper list = 
  case list of
    [] -> []
    x::xs -> (div [] [text x])::(renderCorpusHelper xs)


renderAddText : Html Message
renderAddText = div []
                  [ input [onInput SaveDraft] []
                  , button [onClick AddText] [text "Add Text"]
                  ]