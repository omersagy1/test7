module Game.Printer exposing (..)

import Time exposing (Time)

import Common.Animation as Animation
import Common.Annex exposing (..)
import Game.Model as Model exposing (Model, ScrollingMessage)


update : Time -> Model -> Model
update timePassed model =
  case model.activeScrollingMessage of
    Nothing -> model
    Just msg -> 
      let
        updatedMsg = { msg | animation = Animation.update timePassed msg.animation }
      in
        { model | activeScrollingMessage = Just updatedMsg }
        |> endIfComplete


setActiveMessage : String -> Model -> Model
setActiveMessage msg model = 
  { model | activeScrollingMessage = Just (scrollingMessage msg) }


endIfComplete : Model -> Model
endIfComplete model =
  case model.activeScrollingMessage of
    Nothing -> model
    Just msg -> 
      if not (scrollComplete msg) then model
      else
            { model | activeScrollingMessage = Nothing
                    , messageHistory = (msg.fullText)::model.messageHistory }


isPrinting : Model -> Bool
isPrinting model = isNothing model.activeScrollingMessage


allMessages : Model -> List String
allMessages m = (maybeToList (activeMessage m)) ++ (messageHistory m)


-- All messages already fully printed.
messageHistory : Model -> List String
messageHistory m = m.messageHistory


activeMessage : Model -> Maybe String
activeMessage m =
  case m.activeScrollingMessage of
    Nothing -> Nothing
    Just scrollingMessage ->
      Just (currentText scrollingMessage)


scrollingMessage : String -> ScrollingMessage
scrollingMessage msg =
  { fullText = msg
  , animation = Animation.init (scrollTime msg)
  }


scrollComplete : ScrollingMessage -> Bool
scrollComplete msg = Animation.complete msg.animation


currentText : ScrollingMessage -> String
currentText sm =
  let
    numChars = (toFloat (String.length (sm.fullText))) * (Animation.currentValue sm.animation)
  in
    String.left (round numChars) sm.fullText


timePerCharacter : Time
timePerCharacter = 0.1 * Time.second


scrollTime : String -> Time
scrollTime s = (toFloat (String.length s)) * timePerCharacter