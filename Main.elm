import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)
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

  , gameTime: Time
  , paused: Bool
  }

model : Model
model = 
  { val = 0
  , corpus = []
  , display = []
  , textDraft = ""

  , gameTime = 0
  , paused = True
  }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Message
subscriptions model =
  AnimationFrame.diffs UpdateTime


-- UPDATE

type Message = UpdateTime Time
               | AddText
               | SaveDraft String
               | Play
               | Pause


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let newModel = case msg of

      UpdateTime t -> gameLoop model t

      AddText -> { model | corpus = model.corpus ++ [model.textDraft]
                         , textDraft = "" }

      SaveDraft txt -> { model | textDraft = txt }

      Play -> { model | paused = False }

      Pause -> { model | paused = True }

  in
    (newModel, Cmd.none)
      

gameLoop : Model -> Time -> Model
gameLoop model timePassed =
  if model.paused then
    model
  else
    { model | gameTime = model.gameTime + timePassed }


-- VIEW

view : Model -> Html Message
view model = div []
                 [ corpusHeader
                 , renderCorpus model.corpus
                 , renderAddText model.textDraft
                 , renderPlay
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


renderAddText : String -> Html Message
renderAddText draft = 
  div [ style[("margin", "5px")] ]
      [ input [ onInput SaveDraft
                , value draft
              ] 
              []
      , button [onClick AddText] [text "Add Text"]
      ]


renderPlay : Html Message
renderPlay = div [style[("margin", "5px")]] 
                 [button [style [("margin-right", "5px")]
                         ,onClick Play] 
                         [text "PLAY"]
                 ,button [onClick Pause] [text "PAUSE"]]