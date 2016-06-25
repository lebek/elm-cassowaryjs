import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import List.Nonempty as NE exposing (Nonempty, (:::))
import String
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

type alias Vertex = {
    position: Position
  , vars: { x: Variable, y: Variable }
}

type alias Poly = Nonempty Vertex

type alias Model =
    { outer : Poly
    --, inner : Poly
    , drag : Maybe Drag
    , solver : Solver
    }

type alias Drag =
    { start : Position
    , current : Position
    , vertex : Vertex
    }

topLeft = {
  position = { x = 30, y = 30 },
  vars = { x = makeVariable 30, y = makeVariable 30 }
  }

topRight = {
  position = { x = 100, y = 30 },
  vars = { x = makeVariable 100, y = makeVariable 30 }
  }

bottomRight = {
  position = { x = 100, y = 100 },
  vars = { x = makeVariable 100, y = makeVariable 100 }
  }

bottomLeft = {
  position = { x = 30, y = 100 },
  vars = { x = makeVariable 30, y = makeVariable 100 }
  }

outerPoly =
  topLeft ::: (topRight ::: (bottomRight ::: (NE.fromElement bottomLeft)))

stayOnCanvas : Vertex -> Solver -> Solver
stayOnCanvas v s = addConstraint ((Var v.vars.x) .>=. (Lit 30)) s
  |> addConstraint ((Var v.vars.y) .>=. (Lit 30))
  |> addConstraint ((Var v.vars.x) .<=. (Lit 300))
  |> addConstraint ((Var v.vars.y) .<=. (Lit 300))

initialSolver = makeSolver
  |> addPointStays [
      (topLeft.vars.x, topLeft.vars.y),
      (topRight.vars.x, topRight.vars.y),
      (bottomRight.vars.x, bottomRight.vars.y),
      (bottomLeft.vars.x, bottomLeft.vars.y)
    ]
  |> addConstraint ((Var topLeft.vars.x) .+. (Lit 30) .<=. (Var topRight.vars.x))
  |> addConstraint ((Var topLeft.vars.y) .+. (Lit 30) .<=. (Var bottomLeft.vars.y))
  |> addConstraint ((Var topLeft.vars.x) .+. (Lit 30) .<=. (Var bottomRight.vars.x))
  |> addConstraint ((Var topLeft.vars.y) .+. (Lit 30) .<=. (Var bottomRight.vars.y))
  |> addConstraint ((Var topRight.vars.y) .+. (Lit 30) .<=. (Var bottomLeft.vars.y))
  |> addConstraint ((Var topRight.vars.y) .+. (Lit 30) .<=. (Var bottomRight.vars.y))
  |> addConstraint ((Var bottomLeft.vars.x) .+. (Lit 30) .<=. (Var topRight.vars.x)) -- not sure
  |> addConstraint ((Var bottomLeft.vars.x) .+. (Lit 30) .<=. (Var bottomRight.vars.x)) -- not sure
  |> (\s -> List.foldr stayOnCanvas s [topLeft, topRight, bottomRight, bottomLeft])

init : ( Model, Cmd Msg )
init =
  ( Model outerPoly Nothing initialSolver, Cmd.none )



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

closestVertex : Poly -> Position -> Vertex
closestVertex poly position =
  let
    f v minv = if (distance v.position position) < (distance minv.position position) then v else minv
  in
  List.foldr f (NE.head poly) (NE.tail poly)

updateHelp : Msg -> Model -> Model
updateHelp msg ({outer, drag, solver} as model) =
  case msg of
    DragStart xy ->
      let
        -- d = Debug.log "DragStart" ()
        closest = closestVertex outer xy
        updatedSolver = addEditVar closest.vars.x solver
            |> addEditVar closest.vars.y
            |> beginEdit
      in
        Model outer (Just <| Drag xy xy closest) updatedSolver

    DragAt xy ->
      let
        -- d = Debug.log "DragAt" xy
        updateSolver {start,current,vertex} = suggestValue vertex.vars.x (toFloat (vertex.position.x + current.x - start.x)) solver
          |> suggestValue vertex.vars.y (toFloat (vertex.position.y + current.y - start.y))
          |> solve
      in case drag of
        Just d -> updateSolver d |>
          (\s -> Model outer (Just <| Drag d.start xy d.vertex) s)

        Nothing ->
          Model outer Nothing solver

    DragEnd _ ->
      let
        d = Debug.log "DragEnd" ()
      in
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

viewPoly : Poly -> Svg Msg
viewPoly poly =
    polygon [
      A.points <| String.join " " (NE.toList <| NE.map (\v -> (toString v.position.x) ++ "," ++ (toString v.position.y)) poly),
      A.stroke "blue",
      A.strokeWidth "2",
      --A.fill "transparent"
      A.fill "rgba(0,0,255,0.1)"
      ] []

view : Model -> Html Msg
view model =
  let
    realPoly =
      getPosition model
  in
    Svg.svg
      [ A.width "800", A.height "800", A.viewBox "0 0 800 800" ]
      ([viewPoly realPoly] ++ (NE.toList <| NE.map (\p -> viewPosition p.position False) realPoly))

px : Int -> String
px number =
  toString number ++ "px"


getPosition : Model -> Poly
getPosition {outer, drag, solver} =
  let
    --gridRound x = (round <| x / 20) * 20
    f vertex = { vertex | position = {
        x = (round <| getValue vertex.vars.x solver)
      , y = (round <| getValue vertex.vars.y solver)
      }
    }
  in
  case drag of
    Nothing ->
      outer

    Just {start,current} -> NE.map f outer



onMouseDown : Svg.Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
