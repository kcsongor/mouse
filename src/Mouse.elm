effect module Mouse where { subscription = MySub } exposing
  ( MouseState, Position, Scroll, MouseButton(..)
  , clicks
  , moves
  , downs, ups
  , wheel
  )

{-| Experimental package extending the mouse API with additional events,
    such as:

  - Handling right and middle buttons (probably want to disable the context menu,
      which is outside of the scope of this package for now)
  - Scroll events
  - Detecting if ctrl was pressed during the event

# Mouse Event
@docs MouseState, Position, Scroll, MouseButton

# Subscriptions
@docs clicks, moves, downs, ups, wheel

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json exposing ((:=), succeed, andThen, map)
import Process
import Task exposing (Task)



-- POSITIONS


{-| The position of the mouse relative to the whole document. So if you are
scrolled down a bunch, you are still getting a coordinate relative to the
very top left corner of the *whole* document.
-}
type alias Position =
  { x : Int
  , y : Int
  }

{-| The mouse buttons that are parsed from the event
-}
type MouseButton
  = Left
  | Middle
  | Right

{-| Scroll offsets for the scroll events.
-}
type alias Scroll =
  { dx : Int
  , dy : Int
  , dz : Int
  }

{-| An aggregate of the different information parsed from the event.
This is currently not ideal, as it requires extra logic on the consumer side
to determine which even happened, so this is likely to change.

MouseButton can Nothing, if the event doesn't correspond to any of the 3
major buttons (right, middle, left)
-}
type alias MouseState =
  { button   : Maybe MouseButton
  , position : Position
  , ctrlKey  : Bool
  , scroll   : Maybe Scroll
  }

(>>=) = andThen

{-| The decoder used to extract a `MouseState` from a JavaScript mouse event.
-}
mousestate : Json.Decoder MouseState
mousestate =
  Json.object4 MouseState (("which" := Json.int) >>= (\button ->
                              let b = case button of
                                  1 -> Just Left
                                  2 -> Just Middle
                                  3 -> Just Right
                                  _ -> Nothing
                              in succeed b))
                          position
                          ("ctrlKey" := Json.bool)
                          (Json.maybe scroll)


position : Json.Decoder Position
position =
  Json.object2 Position ("pageX" := Json.int) ("pageY" := Json.int)

scroll : Json.Decoder Scroll
scroll =
  Json.object3 Scroll ("deltaX" := Json.int)
                      ("deltaY" := Json.int)
                      ("deltaZ" := Json.int)

-- MOUSE EVENTS


{-| Subscribe to mouse clicks anywhere on screen.
-}
clicks : (MouseState -> msg) -> Sub msg
clicks tagger =
  subscription (MySub "click" tagger)


{-| Subscribe to mouse moves anywhere on screen. It is best to unsubscribe if
you do not need these events. Otherwise you will handle a bunch of events for
no benefit.
-}
moves : (MouseState -> msg) -> Sub msg
moves tagger =
  subscription (MySub "mousemove" tagger)

{-| Subscribe to the wheel events.
-}
wheel : (MouseState -> msg) -> Sub msg
wheel tagger =
  subscription (MySub "wheel" tagger)


{-| Get a position whenever the user *presses* the mouse button.
-}
downs : (MouseState -> msg) -> Sub msg
downs tagger =
  subscription (MySub "mousedown" tagger)


{-| Get a position whenever the user *releases* the mouse button.
-}
ups : (MouseState -> msg) -> Sub msg
ups tagger =
  subscription (MySub "mouseup" tagger)



-- SUBSCRIPTIONS


type MySub msg
  = MySub String (MouseState -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
  MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  Dict.Dict String (Watcher msg)


type alias Watcher msg =
  { taggers : List (MouseState -> msg)
  , pid : Process.Id
  }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
  Dict.Dict String (List (MouseState -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
  categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
  case subs of
    [] ->
      subDict

    MySub category tagger :: rest ->
      categorizeHelp rest <|
        Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
  case maybeValues of
    Nothing ->
      Just [value]

    Just values ->
      Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed Dict.empty


type alias Msg =
  { category : String
  , mousestate : MouseState
  }


(&>) t1 t2 = t1 `Task.andThen` \_ -> t2


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    leftStep category {pid} task =
      Process.kill pid &> task

    bothStep category {pid} taggers task =
      task
        `Task.andThen` \state ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)

    rightStep category taggers task =
      task
        `Task.andThen` \state ->

      Process.spawn (Dom.onWindow category mousestate (Platform.sendToSelf router << Msg category))
        `Task.andThen` \pid ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)
  in
    Dict.merge
      leftStep
      bothStep
      rightStep
      oldState
      (categorize newSubs)
      (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router {category,mousestate} state =
  case Dict.get category state of
    Nothing ->
      Task.succeed state

    Just {taggers} ->
      let
        send tagger =
          Platform.sendToApp router (tagger mousestate)
      in
        Task.sequence (List.map send taggers)
          `Task.andThen` \_ ->

        Task.succeed state
