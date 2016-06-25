import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)

import Svg exposing (..)
import Svg.Attributes as A

import Cassowary exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Bar = {
    start: Position
  , end: Position
  }

type alias Model =
    { bar : Bar
    , drag : Maybe Drag
    , solver : Solver
    }

type Control = Start | End

type alias Drag =
    { start : Position
    , current : Position
    , control : Control
    }

sx = makeVariable 50
sy = makeVariable 50
ex = makeVariable 300
ey = makeVariable 300
cx = makeVariable 0
cy = makeVariable 0


initialSolver = makeSolver
  |> addPointStays [(ex, ey), (sx, sy)]
  |> addConstraint ((Var sx) .<=. (Var ex))
  |> addConstraint ((Var sy) .<=. (Var ey))
  |> addConstraint ((Var cx) .=. (Var sx) .*. 0.5 .+. (Var ex) .*. 0.5)
  |> addConstraint ((Var cy) .=. (Var sy) .*. 0.5 .+. (Var ey) .*. 0.5)
  --|> solve

init : ( Model, Cmd Msg )
init =
  ( Model {
    start=(Position (round <| getValue sx initialSolver) (round <| getValue sy initialSolver))
    , end=(Position (round <| getValue ex initialSolver) (round <| getValue ey initialSolver))
  } Nothing initialSolver, Cmd.none )



-- UPDATE


type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )

distance : Position -> Position -> Float
distance a b =
  let
    f = toFloat
  in
  sqrt (((f a.x)-(f b.x))^2 + ((f a.y)-(f b.y))^2)

closestControl : Bar -> Position -> Control
closestControl bar position =
  let
    startDist = distance bar.start position
    endDist = distance bar.end position
  in if startDist < endDist then Start else End

updateHelp : Msg -> Model -> Model
updateHelp msg ({bar, drag, solver} as model) =
  case msg of
    DragStart xy ->
      let
        d = Debug.log "DragStart" ()
        control = closestControl bar xy
        updatedSolver = case control of
          End -> addEditVar ex solver
            |> addEditVar ey
            |> beginEdit
            -- |> solve
          Start -> addEditVar sx solver
            |> addEditVar sy
            |> beginEdit
            -- |> solve
      in
        Model bar (Just <| Drag xy xy control) updatedSolver

    DragAt xy ->
      let
        -- d = Debug.log "DragAt" xy
        newStart start current = suggestValue sx (toFloat (bar.start.x + current.x - start.x)) solver
          |> suggestValue sy (toFloat (bar.start.y + current.y - start.y))
          |> solve
        newEnd start current = suggestValue ex (toFloat (bar.end.x + current.x - start.x)) solver
          |> suggestValue ey (toFloat (bar.end.y + current.y - start.y))
          |> solve
        updateSolver {start,current,control} = case control of
          Start -> newStart start current
          End -> newEnd start current
      in case drag of
        Just d -> updateSolver d |>
          (\s -> Model bar (Just <| Drag d.start xy d.control) s)

        Nothing ->
          Model bar Nothing solver

    DragEnd _ ->
      let d = Debug.log "DragEnd" () in
      Model (getPosition model) Nothing (endEdit solver)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)

viewPosition : Position -> Bool -> Svg Msg
viewPosition position editing =
    let
      fill = if editing then "red" else "transparent"
    in
    circle [
      onMouseDown,
      A.cx (toString <| position.x),
      A.cy (toString <| position.y),
      A.r <| toString 10,
      A.stroke "blue",
      A.strokeWidth "2",
      A.fill fill
      ] []

viewCenter : Position -> Float -> Svg Msg
viewCenter position radius =
    circle [
      A.cx (toString <| position.x),
      A.cy (toString <| position.y),
      A.r <| toString radius,
      A.stroke "blue",
      A.strokeWidth "2",
      A.fill "transparent"
      ] []

viewBar : Position -> Position -> Svg Msg
viewBar pos1 pos2 =
    line [
      A.x1 (toString pos1.x),
      A.y1 (toString pos1.y),
      A.x2 (toString pos2.x),
      A.y2 (toString pos2.y),
      A.stroke "blue",
      A.strokeWidth "2"
      ] []

view : Model -> Html Msg
view model =
  let
    realBar =
      getPosition model
    center = Position (round <| getValue cx model.solver) (round <| getValue cy model.solver)
    radius = (distance realBar.start realBar.end) / 2
    --d = Debug.log "bar" realBar
    editingStart = Maybe.withDefault False (Maybe.map (\d -> d.control == Start) model.drag)
    editingEnd = Maybe.withDefault False (Maybe.map (\d -> d.control == End) model.drag)
  in
    Svg.svg
      [ A.width "800", A.height "800", A.viewBox "0 0 800 800" ]
      [
        viewBar realBar.start realBar.end,
        --viewCenter center radius,
        --viewCenter center 5,
        viewPosition realBar.start editingStart,
        viewPosition realBar.end editingEnd
      ]

px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Bar
getPosition {bar, drag, solver} =
  let
    gridRound x = (round <| x / 20) * 20
    newStart start current = Position (gridRound <| getValue sx solver) (gridRound <| getValue sy solver)
    newEnd start current = Position (gridRound <| getValue ex solver) (gridRound <| getValue ey solver)
  in
  case drag of
    Nothing ->
      bar

    Just {start,current} ->
      {
        start = newStart start current,
        end = newEnd start current
        }



onMouseDown : Svg.Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
