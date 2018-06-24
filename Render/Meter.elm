module Render.Meter exposing (meter)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Attributes exposing (style, value, css)


w = 200
h = 30
margin = 5


meter : Float -> String -> Html a
meter fractionFilled labelText = 
  div [css [ width (px w)
           , height (px h)
           , backgroundColor Colors.black
           , marginTop (px margin)
           , marginBottom (px margin)
           ]
      ] 
      [ bar fractionFilled
      , label labelText
      ]


bar : Float -> Html a
bar fractionFilled =
  div [css [ width (px (toFloat(w) * fractionFilled))
           , height (px h)
           , backgroundColor Colors.gray
           , position absolute
           ]
      ]
      []


label : String -> Html a
label t =
  div [css [ width (px w)
           , height (px h)
           , position absolute 
           , displayFlex
           , alignItems center
           , justifyContent center
           , color Colors.white
           ]
      ]
      [text t]