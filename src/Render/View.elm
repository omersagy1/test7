module Render.View exposing (view)

import Css exposing (..)
import Css.Colors as Colors
import Html.Styled exposing (Html, button, div, text, input)
import Html.Styled.Attributes exposing (style, value, css)
import Html.Styled.Events exposing (onClick, onInput)

import Common.Annex exposing(..)

import Game.Model exposing (Model)
import Game.Update exposing (Message)

import Render.Constants as Constants
import Render.Choice as Choice
import Render.Messages as Messages
import Render.Meter as Meter
import Render.ViewModel as ViewModel exposing (ViewModel)


view : Model -> Html Message
view model = renderViewModel (ViewModel.init model)


renderViewModel : ViewModel -> Html Message
renderViewModel model =
    div [ css [ displayFlex 
              , height (pct 100)
              , padding (px 30)
              , backgroundColor Colors.black
              , color Colors.white
              , fontFamily serif
              ] 
        ]
        [ Messages.messageHistory model.messageHistory
        , interactiveDisplay model
        ]


interactiveDisplay : ViewModel -> Html Message
interactiveDisplay model =
  div [ css [flexGrow (num 1)] ]
      (concatMaybes
        [ Choice.choiceButtons model.choiceButtons
        , Just (fire model.fireMeter)
        , actionMeters model.actionMeters
        , resources model.resourceDisplays
        ])


fire : ViewModel.Meter -> Html Message
fire fireMeter =
  div [ onClick fireMeter.callback ]
      [ Meter.new
        |> Meter.fractionFilled fireMeter.proportion
        |> Meter.setLabel fireMeter.label
        |> Meter.active fireMeter.clickable
        |> Meter.colorBorderOnActive Constants.inactiveBorderColor 
                                     Constants.activeBorderColor 
        |> Meter.colorBarOnFraction Constants.deadFireColor Constants.fullFireColor
        |> Meter.render
      ]


resources : List (ViewModel.ResourceDisplay) -> Maybe (Html Message)
resources resourceDisplays =
  case resourceDisplays of
    [] -> Nothing
    rs -> 
      div [] (List.map resource rs)
      |> Just


resource : ViewModel.ResourceDisplay -> Html a
resource r =
  let
    labelText = r.name ++ ": " ++ (toString r.amount)
  in
    div [css [marginBottom (px 10)]]
        [text labelText]


actionMeters : List (ViewModel.Meter) -> Maybe (Html Message)
actionMeters actions =
  case actions of
    [] -> Nothing
    actions -> 
      div [] (List.map actionMeter actions)
      |> Just


actionMeter : ViewModel.Meter -> Html Message
actionMeter action =
  div [ onClick action.callback ]
      [ Meter.new 
        |> Meter.fractionFilled action.proportion
        |> Meter.setLabel action.label
        |> Meter.active action.clickable
        |> Meter.colorBorderOnActive Constants.inactiveBorderColor 
                                     Constants.activeBorderColor 
        |> Meter.render
      ]
