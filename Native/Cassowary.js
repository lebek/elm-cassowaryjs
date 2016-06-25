var _user$project$Native_Cassowary = function (elm) {
  var Utils = _elm_lang$core$Native_Utils;
  var solvers = {};
  var variables = {};

  function makeSolver () {
    var solverGuid = Utils.guid();
    var solver = new c.SimplexSolver();
    // bugs when this is false, ("jumping" variables when switching edit var)
    //solver.autoSolve = false;
    solvers[solverGuid] = solver;
    return solverGuid;
  }

  function makeVariable (value) {
    var variableGuid = Utils.guid();
    variables[variableGuid] = new c.Variable({ value: value });
    return variableGuid;
  }

  function getValue (_variable) {
    return variables[_variable].value;
  }

  function _makeExpr(expr) {
    if (expr.ctor === 'Add') {
      return c.plus(_makeExpr(expr._0), _makeExpr(expr._1));
    } else if (expr.ctor === 'Coeff') {
      return c.times(_makeExpr(expr._0), expr._1);
    } else if (expr.ctor === 'Var') {
      return variables[expr._0];
    } else if (expr.ctor === 'Lit') {
      return expr._0;
    }
  }

  function addConstraint(_solver, equation) {
    var solver = solvers[_solver];
    var lhs = _makeExpr(equation._0);
    var rhs = _makeExpr(equation._1);
    if (equation.ctor === 'LteInequality') {
      solver.addConstraint(new c.Inequality(lhs, c.LEQ, rhs));
    } else if (equation.ctor === 'GteInequality') {
      solver.addConstraint(new c.Inequality(lhs, c.GEQ, rhs));
    } else if (equation.ctor === 'Equality') {
      solver.addConstraint(new c.Equation(lhs, rhs));
    }
    return _solver;
  }

  function solve(_solver) {
    var solver = solvers[_solver];
    solver.resolve();
    return _solver;
  }

  function addEditVar(_solver, _variable) {
    var solver = solvers[_solver];
    var variable = variables[_variable];
    solver.addEditVar(variable);
    return _solver;
  }

  function addStay(_solver, _variable) {
    var solver = solvers[_solver];
    var variable = variables[_variable];
    solver.addStay(variable);
    return _solver;
  }

  function addPointStays(_solver, _points) {
    var solver = solvers[_solver];
    var points = _points.table.map((p) => {
      return { x: variables[p._0], y: variables[p._1] };
    });
    solver.addPointStays(points);
    return _solver;
  }

  function beginEdit(_solver) {
    //console.log('beginEdit');
    var solver = solvers[_solver];
    solver.beginEdit();
    return _solver;
  }

  function endEdit(_solver) {
    //console.log('endEdit');
    var solver = solvers[_solver];
    solver.endEdit();
    return _solver;
  }

  function suggestValue(_solver, _variable, value) {
    //console.log('suggestValue');
    var solver = solvers[_solver];
    var variable = variables[_variable];
    solver.suggestValue(variable, value);
    return _solver;
  }

  return {
    makeSolver: makeSolver,
    makeVariable: makeVariable,
    getValue: getValue,
    addConstraint: F2(addConstraint),
    solve: solve,
    addEditVar: F2(addEditVar),
    addStay: F2(addStay),
    beginEdit: beginEdit,
    endEdit: endEdit,
    suggestValue: F3(suggestValue),
    addPointStays: F2(addPointStays)
  };
}();
