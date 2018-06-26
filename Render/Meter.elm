module Render.Meter exposing (meter)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)


meterLength : Float
meterLength = 200

meterHeight : Float
meterHeight = 30

margin : Float
margin = 12

activeBorderColor : Color
activeBorderColor = Colors.yellow

inactiveBorderColor : Color
inactiveBorderColor = (rgb 60 60 60)


meter : Float -> String -> Bool -> Html a
meter fractionFilled labelText isActive = 
  let
    borderC = if isActive then activeBorderColor else inactiveBorderColor
  in
    div [css [ width (px meterLength)
            , height (px meterHeight)
            , backgroundColor Colors.black
            , marginTop (px margin)
            , marginBottom (px margin)
            , borderColor borderC
            , borderWidth (px 3)
            , borderStyle solid
            ]
        ] 
        [ bar fractionFilled
        , label labelText
        ]


bar : Float -> Html a
bar fractionFilled =
  div [css [ width (px <| meterLength * fractionFilled)
           , height (px meterHeight)
           , backgroundColor Colors.gray
           , position absolute
           ]
      ]
      []


label : String -> Html a
label t =
  div [css [ width (px meterLength)
           , height (px meterHeight)
           , position absolute 
           , displayFlex
           , alignItems center
           , justifyContent center
           , color Colors.white
           ]
      ]
      [text t]