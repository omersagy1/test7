module Render.Messages exposing (messageHistory)

import Css exposing (..)
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)

import Annex exposing(..)


displaySize : Int
displaySize = 14

displayWidth : Px
displayWidth = (px 350)

spaceBetweenMessages : Px
spaceBetweenMessages = (px 12)

rightMargin : Px
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
