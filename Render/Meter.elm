module Render.Meter exposing (meter)

import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (style, value)


width = 200
height = 30
margin = 5

px : Int -> String 
px n = (toString n) ++ "px"


meter : Float -> String -> Html a
meter fractionFilled labelText = 
  div [style [ ("width", width |> px)
             , ("height", height |> px)
             , ("background-color", "black")
             , ("margin-top", margin |> px)
             , ("margin-bottom", margin |> px)
             ]
      ] 
      [ bar fractionFilled
      , label labelText
      ]


bar : Float -> Html a
bar fractionFilled =
  div [style [ ("width", toFloat(width) * fractionFilled |> round |> px)
             , ("height", height |> px)
             , ("background-color", "grey")
             , ("position", "absolute")
             ]
      ]
      []


label : String -> Html a
label t =
  div [style [ ("width", width |> px)
             , ("height", height |> px)
             , ("position", "absolute")
             , ("display", "flex")
             , ("align-items", "center")
             , ("justify-content", "center")
             , ("color", "white")
             ]]
      [text t]