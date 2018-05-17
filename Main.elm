import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Annex
import AnimationFrame
import Time exposing (Time, second)


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
  let newModel =
    case msg of

      UpdateTime t -> model

      AddText ->
        { model | corpus = model.corpus ++ [model.textDraft] }

      SaveDraft t ->
        { model | textDraft = t }

      Run -> model

      Pause -> model

  in
    (newModel, Cmd.none)
      


-- VIEW

view : Model -> Html Message
view model =
  div 
    []
    ((renderCorpus model.corpus) ++ [renderAddText])

renderCorpus : List String -> List (Html a)
renderCorpus = List.reverse << renderCorpusHelper 

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