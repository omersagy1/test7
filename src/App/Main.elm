import Html.Styled
import Navigation
import Task
import Time

import App.Model exposing (..)
import App.View
import Editor.Main
import Game.Model
import Game.Update
import Game.Subs


main : Program Never Model Message
main =
  Navigation.program UrlChange
    { init = init
    , view = App.View.view >> Html.Styled.toUnstyled
    , update = update
    , subscriptions = subscriptions
    }


init : Navigation.Location -> (Model, Cmd Message)
init location = 
  update (UrlChange location) model


model : Model
model =
  { currentPage = Editor
  , editorModel = Editor.Main.initialModel
  , gameModel = Game.Model.initialModel
  }


subscriptions : Model -> Sub Message
subscriptions model =
  case model.currentPage of
    Editor -> Sub.none
    Game -> Sub.map GameMessage (Game.Subs.subscriptions model.gameModel)


update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case (msg, model.currentPage) of

    (EditorMessage m, Editor) -> 
      ({ model | editorModel = Editor.Main.update m model.editorModel }
      , Cmd.none)

    (GameMessage gameMsg, Game) -> 
      ({ model | gameModel = Game.Update.update gameMsg model.gameModel }
      , Cmd.map GameMessage (Game.Update.command gameMsg))

    (SwitchPage, anyPage) -> 
      switchPage model

    (Navigate page, anyPage) ->
      navigateToPage page model
    
    (UrlChange location, anyPage) ->
      handleUrlChange location model

    other ->
      -- Somehow a message was sent for the wrong page.
      -- Drop it and do nothing.
      -- TODO: figure out how to send warnings.
      (model, Cmd.none)


navigateToPage : AppPage -> Model -> (Model, Cmd Message)
navigateToPage page model =
  case page of
    Editor -> (model, Navigation.newUrl (pathForPage Game))
    Game -> (model, Navigation.newUrl (pathForPage Editor))


switchPage : Model -> (Model, Cmd Message)
switchPage model =
  case model.currentPage of
    Editor -> update (Navigate Game) model
    Game -> update (Navigate Editor) model


handleUrlChange : Navigation.Location -> Model -> (Model, Cmd Message)
handleUrlChange location model = 
  let
    parts = String.split "/" location.pathname
    lastPart = Maybe.withDefault "" (List.head (List.reverse parts))
    page = Maybe.withDefault Editor (pageForPath lastPart)
  in
    case page of
      Editor -> 
        initializeGame { model | currentPage = Game }
      Game -> 
        initializeEditor { model | currentPage = Editor }


initializeGame : Model -> (Model, Cmd Message)
initializeGame m = (m, Task.perform (GameMessage << Game.Update.StartTime) Time.now)


initializeEditor : Model -> (Model, Cmd Message)
initializeEditor = update (EditorMessage Editor.Main.Initialize)


pathForPage : AppPage -> String
pathForPage page =
  case page of
    Editor -> "editor"
    Game -> "game"


pageForPath : String -> Maybe AppPage
pageForPath p =
  if p == (pathForPage Editor) then Just Editor
  else if p == (pathForPage Game) then Just Game
  else Nothing