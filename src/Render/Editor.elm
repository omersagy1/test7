module Render.Editor exposing (view)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)

import Editor.Main exposing (Model)


view : Model -> Html a
view model = 
  div [ css [ height (pct 100)
            , padding (px 30)
            , backgroundColor Colors.black
            , color Colors.white
            ]
      ]
      [ header
      , actionsSet model
      , actionsUsed model
      , badActionReferences model
      ]


header : Html a
header = div [css [marginBottom (px 20)]] 
             [text "STATIC ANALYSIS CHECKS"]


actionsSet : Model -> Html a
actionsSet model = 
  div [css [marginBottom (px 20)]] 
      ([ text "The following actions were initialized:" ] ++
       List.map entry model.actionsSet)


actionsUsed : Model -> Html a
actionsUsed model =
  div [css [marginBottom (px 20)]] 
      ([ text "The following actions were referenced:" ] ++
       List.map entry model.actionsUsed)


badActionReferences : Model -> Html a
badActionReferences model =
  div [css [marginBottom (px 20)]] 
      ([ text "The following actions references are broken:" ] ++
       List.map entry (Editor.Main.badActionReferences model))


entry : String -> Html a
entry s = div [] [text ("- \"" ++ s ++ "\"")]