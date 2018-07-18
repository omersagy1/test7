module Render.Meter exposing (..)

import Color
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
  , borderWidth : Px
  }

new : Meter
new = Meter
  { width = px Constants.meterLength
  , height = px Constants.meterHeight
  , background = Colors.black
  , barColorFn = (\_ -> Colors.gray)
  , fractionFilled = 1.0
  , active = True
  , label = ""
  , borderColorFn = (\_ -> Colors.yellow)
  , borderWidth = px Constants.borderWidth
  }


fractionFilled : Float -> Meter -> Meter
fractionFilled f (Meter params) = Meter { params | fractionFilled = f }


setLabel : String -> Meter -> Meter
setLabel l (Meter params) = Meter { params | label = l }


active : Bool -> Meter -> Meter
active isActive (Meter params) = Meter { params | active = isActive }


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
colorBarOnFraction emptyColor fullColor (Meter params) =
  Meter { params | barColorFn = 
                    (\(Meter params) -> 
                       interpolateColors emptyColor fullColor params.fractionFilled) }


interpolateColors : Color -> Color -> Float -> Color
interpolateColors begin end frac =
  let
    factor = min 1 (max 0 frac)
    inverse = 1 - frac
    mult : Int -> Float -> Int
    mult x y = Basics.round ((toFloat x) * y)
  in
    rgb ((mult begin.red inverse) + (mult end.red factor))
        ((mult begin.green inverse) + (mult end.green factor))
        ((mult begin.blue inverse) + (mult end.blue factor))


margin : Float
margin = 12


render : Meter -> Html a
render (Meter params) = 
  let
    borderC = params.borderColorFn (Meter params)
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
        [ bar params.fractionFilled (params.barColorFn (Meter params))
        , label params.label
        ]


bar : Float -> Color -> Html a
bar fractionFilled c =
  div [css [ width (px <| Constants.meterLength * fractionFilled)
           , height (px Constants.meterHeight)
           , backgroundColor c
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