module Cassowary exposing ((.+.), (.*.), (.=.), (.<=.), (.>=.),
  makeSolver, addConstraint, makeVariable, getValue, solve, addEditVar, addStay,
  beginEdit, suggestValue, endEdit, addPointStays, Solver, Variable, Expression(..))

import Html exposing (..)
import Native.Cassowary
import Array


-----
type Expression
    = Var Variable
    | Lit Float
    | Coeff Expression Float
    | Add Expression Expression

type Equation
    = Equality Expression Expression
    | LteInequality Expression Expression
    | GteInequality Expression Expression

(.+.) : Expression -> Expression -> Expression
(.+.) =
    Add


(.*.) : Expression -> Float -> Expression
(.*.) =
    Coeff


(.=.) : Expression -> Expression -> Equation
(.=.) x y =
    Equality x y


(.<=.) : Expression -> Expression -> Equation
(.<=.) x y =
    LteInequality x y


(.>=.) : Expression -> Expression -> Equation
(.>=.) x y =
    GteInequality x y


infixr 7 .<=.


infixr 7 .>=.


infixr 7 .=.


infixr 8 .+.


infixr 9 .*.
-----

-- type NotEditing = NotEditing
-- type Editing = Editing
type Solver a = Solver
type Variable a = Variable

makeSolver : Solver
makeSolver = Native.Cassowary.makeSolver ()

addConstraint : Equation -> Solver -> Solver
addConstraint equation solver = Native.Cassowary.addConstraint solver equation

makeVariable : Float -> Variable
makeVariable value = Native.Cassowary.makeVariable value

-- todo, actually use solver
getValue : Variable -> Solver -> Float
getValue variable solver = Native.Cassowary.getValue variable

solve : Solver -> Solver
solve solver = Native.Cassowary.solve solver

addEditVar : Variable -> Solver -> Solver
addEditVar variable solver = Native.Cassowary.addEditVar solver variable

addStay : Variable -> Solver -> Solver
addStay variable solver = Native.Cassowary.addEditVar solver variable

addPointStays : List (Variable, Variable) -> Solver -> Solver
addPointStays points solver = Native.Cassowary.addPointStays solver (Array.fromList points)

beginEdit : Solver -> Solver
beginEdit solver = Native.Cassowary.beginEdit solver

suggestValue : Variable -> Float -> Solver -> Solver
suggestValue variable value solver =
  Native.Cassowary.suggestValue solver variable value

endEdit : Solver -> Solver
endEdit solver = Native.Cassowary.endEdit solver

x = makeVariable 5
y = makeVariable 7
z = makeVariable 10

-- main = let
--     solver = addConstraint makeSolver ((Var x) .+. (Var y) .<=. (Lit 4))
--   in
--   text <| toString <| List.map (getValue solver) [x, y]
