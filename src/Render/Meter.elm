module Render.Meter exposing (meter)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)

import Render.Constants as Constants

type Meter = Meter Params

type alias Params =
  { width : Px
  , height : Px
  , background : Color
  , barColorFn : Meter -> Color
  , fractionFilled : Float
  , active : Bool
  , label : String
  , borderColorFn : Meter -> Color
  }

new : Meter
new = Meter
  { width = px 200
  , height = px 50
  , background = Colors.black
  , barColorFn = (\_ -> Colors.gray)
  , fractionFilled = 1.0
  , active = True
  , label = "default"
  , borderColorFn = (\_ -> Colors.yellow)
  }

setBorderColorFn : (Meter -> Color) -> Meter -> Meter
setBorderColorFn fn (Meter params) =
  Meter { params | borderColorFn = fn }


colorBorderOnActive : Color -> Color -> Meter -> Meter
colorBorderOnActive inactiveColor activeColor (Meter params) =
  setBorderColorFn 
    (\(Meter params) -> if params.active then activeColor
                        else inactiveColor)
    (Meter params)


fixedBarColor : Color -> Meter -> Meter
fixedBarColor c (Meter params) =
  Meter { params | barColorFn = (\_ -> c) }


-- Interpolate between two colors based on the fraction
-- of the bar that is filled.
colorBarOnFraction : Color -> Color -> Meter -> Meter
colorBarOnFraction emptyColor fullColor (Meter params) = Meter params


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
    div [css [ width (px Constants.meterLength)
            , height (px Constants.meterHeight)
            , backgroundColor Colors.black
            , marginTop (px margin)
            , marginBottom (px margin)
            , borderColor borderC
            , borderWidth (px Constants.borderWidth)
            , borderStyle solid
            ]
        ] 
        [ bar fractionFilled
        , label labelText
        ]


bar : Float -> Html a
bar fractionFilled =
  div [css [ width (px <| Constants.meterLength * fractionFilled)
           , height (px Constants.meterHeight)
           , backgroundColor Colors.gray
           , position absolute
           ]
      ]
      []


label : String -> Html a
label t =
  div [css [ width (px Constants.meterLength)
           , height (px Constants.meterHeight)
           , position absolute 
           , displayFlex
           , alignItems center
           , justifyContent center
           , color Colors.white
           ]
      ]
      [text t]