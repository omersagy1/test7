module Render.Constants exposing (..)


import Css exposing (..)
import Css.Colors as Colors


meterLength : Float
meterLength = 200

meterHeight : Float
meterHeight = 30

borderWidth : Float
borderWidth = 3

choiceButtonMargin : Float
choiceButtonMargin = 10

choiceButtonLength : Float
choiceButtonLength = meterLength

choiceButtonHeight : Float
choiceButtonHeight = meterHeight

activeBorderColor : Color
activeBorderColor = Colors.yellow

inactiveBorderColor : Color
inactiveBorderColor = (rgb 60 60 60)

fullFireColor : Color
fullFireColor = (rgb 226 88 34)

deadFireColor : Color
deadFireColor = (rgb 20 20 20)