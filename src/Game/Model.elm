module Game.Model exposing(..)

import Time exposing (Time)

import Common.Animation exposing (Animation)
import Common.Randomizer as Randomizer exposing (Randomizer)
import Game.Effect exposing (Effect)
import Game.GameState as GameState exposing (GameState)
import Game.Story exposing (Story)
import Game.StoryEvent exposing (StoryEvent, Choice)
import Parser.Main
import Queue.TimedQueue as TimedQueue exposing (TimedQueue)


initialModel : Model
initialModel = 
  { gameState = Parser.Main.initialGameState 
  , messageHistory = []
  , eventQueue = TimedQueue.new
  , story = Parser.Main.story
  , paused = False
  , fastForward = False
  , activeChoices = []
  , activeScrollingMessage = Nothing
  }


type alias Model = 

  -- State of the game on a semantic level; i.e.
  -- gameState only contains things relevant to
  -- the conceptual understanding of the game, not
  -- the state of the machinery.
  { gameState : GameState

  -- Messages to be displayed on-screen.
  , messageHistory : List String 

  -- Events waiting to be executed.
  , eventQueue : TimedQueue StoryEvent

  -- All story events that could be Conditioned.
  , story : Story

  -- Whether the game receives update time events.
  , paused : Bool

  -- Whether the game is sped up.
  , fastForward : Bool

  -- The choice the player must make to continue the game.
  , activeChoices : List Choice

  , activeScrollingMessage : Maybe ScrollingMessage

  }


type alias ScrollingMessage =
  { fullText : String
  , animation : Animation
  }


initialize : Time -> Model -> Model
initialize t m =
  { m | gameState = GameState.initRandomizer t m.gameState }


gameplayPaused : Model -> Bool
gameplayPaused m =
  hardPaused m || softPaused m


hardPaused : Model -> Bool
hardPaused m = m.paused


softPaused : Model -> Bool
softPaused = waitingOnChoice


waitingOnChoice : Model -> Bool
waitingOnChoice m = not (List.isEmpty m.activeChoices)


togglePause : Model -> Model
togglePause m = { m | paused = not m.paused }


toggleFastForward : Model -> Model
toggleFastForward m = { m | fastForward = not m.fastForward }


restart : Bool -> Model
restart paused = 
  let
    fresh = initialModel
  in
    { fresh | paused = paused }


clearActions : Model -> Model
clearActions m = { m | gameState = GameState.clearActions m.gameState }


displayChoices : List Choice -> Model -> Model
displayChoices choices m =
  { m | activeChoices = choices }


applyEffect : Effect -> Model -> Model
applyEffect effect model =
  { model | gameState = GameState.applyEffect effect model.gameState }


setGameState : GameState -> Model -> Model
setGameState s m = { m | gameState = s }


clearActiveChoices : Model -> Model
clearActiveChoices m =
  { m | activeChoices = [] }


updateRandomizer : Randomizer -> Model -> Model
updateRandomizer r m = 
  { m | gameState = GameState.updateRandomizer r m.gameState }


choose : List a -> Model -> (Maybe a, Model)
choose list m =
  let
    (choice, randomizer) = Randomizer.choose list m.gameState.randomizer
  in
    (choice, updateRandomizer randomizer m)


eventQueueEmpty : Model -> Bool
eventQueueEmpty model = (TimedQueue.size model.eventQueue) == 0