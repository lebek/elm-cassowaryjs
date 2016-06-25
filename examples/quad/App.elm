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
  , selected: Bool
}

type alias Poly = Nonempty Vertex

type alias Model =
    { outer : Poly
    , inner : Poly
    , drag : Maybe Drag
    , solver : Solver
    }

type alias Drag =
    { start : Position
    , current : Position
    , closest : (Float, Int, Vertex)
    }

makeVertex x y = {
  position = { x = round <| x, y = round <| y },
  vars = { x = makeVariable x, y = makeVariable y },
  selected = False
  }

topLeft = makeVertex 30 30
topRight = makeVertex 100 30
bottomRight = makeVertex 100 100
bottomLeft = makeVertex 30 100
outerPoly =
  topLeft ::: (topRight ::: (bottomRight ::: (NE.fromElement bottomLeft)))

topMid = makeVertex 60 30
rightMid = makeVertex 100 60
bottomMid = makeVertex 60 100
leftMid = makeVertex 30 60
innerPoly =
  topMid ::: (rightMid ::: (bottomMid ::: (NE.fromElement leftMid)))

allVertex = NE.append outerPoly innerPoly

stayOnCanvas : Vertex -> Solver -> Solver
stayOnCanvas v s = addConstraint ((Var v.vars.x) .>=. (Lit 30)) s
  |> addConstraint ((Var v.vars.y) .>=. (Lit 30))
  |> addConstraint ((Var v.vars.x) .<=. (Lit 500))
  |> addConstraint ((Var v.vars.y) .<=. (Lit 500))

constrainMid mid v1 v2 solver = addConstraint ((Var mid.vars.x) .=. (Var v1.vars.x) .*. 0.5 .+. (Var v2.vars.x) .*. 0.5) solver
  |> addConstraint ((Var mid.vars.y) .=. (Var v1.vars.y) .*. 0.5 .+. (Var v2.vars.y) .*. 0.5)

initialSolver = makeSolver
  |> addPointStays [
      (topLeft.vars.x, topLeft.vars.y)
    , (topRight.vars.x, topRight.vars.y)
    , (bottomRight.vars.x, bottomRight.vars.y)
    , (bottomLeft.vars.x, bottomLeft.vars.y)

    -- , (topMid.vars.x, topMid.vars.y)
    -- , (rightMid.vars.x, rightMid.vars.y)
    -- , (bottomMid.vars.x, bottomMid.vars.y)
    -- , (leftMid.vars.x, leftMid.vars.y)
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
  -- inner
  |> constrainMid topMid topLeft topRight
  |> constrainMid rightMid bottomRight topRight
  |> constrainMid bottomMid bottomLeft bottomRight
  |> constrainMid leftMid bottomLeft topLeft
  --|> solve

init : ( Model, Cmd Msg )
init =
  ( Model outerPoly innerPoly Nothing initialSolver, Cmd.none )



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

-- closestVertex : Poly -> Position -> Vertex
-- closestVertex poly position =
--   let
--     f v minv = if (distance v.position position) < (distance minv.position position) then v else minv
--   in
--   List.foldr f (NE.head poly) (NE.tail poly)


closestVertex : Nonempty Vertex -> Position -> (Float, Int, Vertex)
closestVertex poly position =
    let
      indexed = NE.indexedMap (\i v -> (distance v.position position, i, v)) poly
      f (d, i, v) (mind, mini, minv) =
        if d < mind then (d, i, v) else (mind, mini, minv)
    in
    List.foldr f (NE.head indexed) (NE.tail indexed)

updateHelp : Msg -> Model -> Model
updateHelp msg ({outer, inner, drag, solver} as model) =
  case msg of
    DragStart xy ->
      let
        -- d = Debug.log "DragStart" ()
        (outerDist, outerIdx, outerVertex) = closestVertex outer xy
        (innerDist, innerIdx, innerVertex) = closestVertex inner xy
        select i poly = NE.indexedMap (\vi v -> if vi == i then {v|selected=True} else v) poly
        newOuter = if outerDist <= innerDist then
          select outerIdx outer else outer
        newInner = if outerDist > innerDist then
          select innerIdx inner else inner
        closest = if outerDist <= innerDist then (outerDist, outerIdx, outerVertex) else (innerDist, innerIdx, innerVertex)
        (_,_,vertex) = closest
        updatedSolver = addEditVar vertex.vars.x solver
            |> addEditVar vertex.vars.y
            |> beginEdit
      in
        Model newOuter newInner (Just <| Drag xy xy closest) updatedSolver

    DragAt xy ->
      let
        -- d = Debug.log "DragAt" xy
        updateSolver {start,current,closest} =
          let
            (_,_,vertex) = closest
          in suggestValue vertex.vars.x (toFloat (vertex.position.x + current.x - start.x)) solver
            |> suggestValue vertex.vars.y (toFloat (vertex.position.y + current.y - start.y))
            |> solve
      in case drag of
        Just d -> updateSolver d |>
          (\s -> Model outer inner (Just <| Drag d.start xy d.closest) s)

        Nothing ->
          Model outer inner Nothing solver

    DragEnd _ ->
      let
        d = Debug.log "DragEnd" ()
        (newOuter, newInner) = getPosition model
        deselect = NE.map (\v -> {v|selected=False})
      in
      Model (deselect newOuter) (deselect newInner) Nothing (endEdit solver)



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
      A.stroke "black",
      A.strokeWidth "2",
      A.fill fill
      ] []

viewOutline : Poly -> List (Svg.Attribute Msg) -> Svg Msg
viewOutline poly style =
    polygon ([
      A.points <| String.join " " (NE.toList <| NE.map (\v -> (toString v.position.x) ++ "," ++ (toString v.position.y)) poly)
      ] ++ style) []

viewPoly : Poly -> List (Svg.Attribute Msg) -> Svg Msg
viewPoly poly style =
    Svg.g [] ([viewOutline poly style] ++ (NE.toList <| NE.map (\p -> viewPosition p.position p.selected) poly))

outerPolyStyle =
  [
    A.stroke "blue",
    A.strokeWidth "2",
    A.fill "transparent"
  ]

innerPolyStyle =
  [
    A.stroke "red",
    A.strokeWidth "2",
    A.fill "transparent"
  ]

view : Model -> Html Msg
view model =
  let
    (outer, inner) =
      getPosition model
  in
    Svg.svg
      [ A.width "800", A.height "800", A.viewBox "0 0 800 800" ]
      ([viewPoly outer outerPolyStyle] ++ [viewPoly inner innerPolyStyle])

px : Int -> String
px number =
  toString number ++ "px"

-- (outer, inner)
getPosition : Model -> (Poly, Poly)
getPosition {outer, inner, drag, solver} =
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
      (outer, inner)

    Just {start,current} -> (NE.map f outer, NE.map f inner)



onMouseDown : Svg.Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)
