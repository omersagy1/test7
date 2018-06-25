module Render.Messages exposing (messageHistory)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Annex exposing(..)


displaySize = 8
displayWidth = (px 350)
spaceBetweenMessages = (px 10)
rightMargin = (px 50)


messageHistory : List String -> Html a 
messageHistory msgs =
  let 
    msgsToDisplay = List.take displaySize msgs
    opacities = rangeToZero 1 displaySize
  in
    div [css [ width displayWidth
             , marginRight rightMargin
             ]
        ]
        (List.map2 message msgsToDisplay opacities)


message : String -> Float -> Html a
message msg opacityValue = 
  div [ css [ marginBottom spaceBetweenMessages
            , opacity (num opacityValue)
            ]
      ] 
      [text msg]
