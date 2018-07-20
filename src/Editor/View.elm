module Editor.View exposing (view)

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
            , fontFamilies ["courier new", "courier"]
            ]
      ]
      [ header
      , badActionReferences model
      , unusedActions model
      , actionsSet model
      , actionsUsed model
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
  let
    brokenReferences = List.map entry (Editor.Main.badActionReferences model)
  in
    case brokenReferences of
      [] -> 
        div [css [marginBottom (px 20)]] 
            [text "No broken action references."]
      refs ->
        div [ css [ marginBottom (px 20)
                  , color Colors.red
                  ]
            ] 
            ([ text "The following action references are broken:" ] ++ refs)
       


unusedActions : Model -> Html a
unusedActions model =
  let
    unusedReferences = List.map entry (Editor.Main.unusedActions model)
  in
    case unusedReferences of
      [] -> 
        div [css [marginBottom (px 20)]] 
            [text "No unused action references."]
      refs ->
        div [ css [ marginBottom (px 20)
                  , color Colors.red
                  ]
            ] 
            ([ text "The following actions are never referenced:" ] ++ refs)


entry : String -> Html a
entry s = div [] [text ("- \"" ++ s ++ "\"")]