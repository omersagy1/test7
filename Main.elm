import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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

init : (Model, Cmd Msg)
init = 
  ({ val = 0 },
   Cmd.none)


-- MODEL

type alias Model = { val: Float }

model : Model
model = { val = 0 }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (\t -> UpdateTime t)


-- UPDATE

type Msg = Increment 
           | Decrement
           | Double
           | UpdateTime Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let newModel =
    case msg of
      Increment ->
        { model | val = model.val + 1 }

      Decrement ->
        { model | val = model.val - 1 }

      Double ->
        { model | val = (Annex.double model.val) }

      UpdateTime t ->
        { model | val = model.val + t / 1000 }
  in
    (newModel, Cmd.none)
      


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model.val) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ button [ onClick Double ] [ text "*2" ]
             ]
    , div [ onClick Double ] [ text "double" ]
    ]
