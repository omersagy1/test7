module Render.Meter exposing (meter)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)


width : Int
width = 200

height : Int
height = 30

px : Int -> String 
px n = (toString n) ++ "px"


meter : Float -> Html a
meter fractionFilled = 
  div [style [ ("width", width |> px)
             , ("height", height |> px)
             , ("background-color", "black")
             ]
      ] 
      [bar fractionFilled]


bar : Float -> Html a
bar fractionFilled =
  div [style [ ("width", toFloat(width) * fractionFilled |> round |> px)
             , ("height", height |> px)
             , ("background-color", "grey")
             , ("position", "absolute")
             ]
      ]
      []