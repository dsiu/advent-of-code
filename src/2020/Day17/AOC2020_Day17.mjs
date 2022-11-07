// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as TableclothSet from "tablecloth-rescript/src/TableclothSet.mjs";
import * as TableclothArray from "tablecloth-rescript/src/TableclothArray.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as TableclothComparator from "tablecloth-rescript/src/TableclothComparator.mjs";
import * as Coord_V3$AdventOfCode from "../../Coord_V3.mjs";
import * as Coord_V4$AdventOfCode from "../../Coord_V4.mjs";
import * as FP_Utils$AdventOfCode from "../../FP_Utils.mjs";

function log(prim) {
  console.log(prim);
}

function compare(a, b) {
  if (a.TAG === /* Coord_V3 */0) {
    if (b.TAG === /* Coord_V3 */0) {
      return Coord_V3$AdventOfCode.compare(a._0)(b._0);
    } else {
      return Pervasives.failwith("Invalid comparison");
    }
  } else if (b.TAG === /* Coord_V3 */0) {
    return Pervasives.failwith("Invalid comparison");
  } else {
    return Coord_V4$AdventOfCode.compare(a._0)(b._0);
  }
}

var include = TableclothComparator.Make({
      compare: compare
    });

var comparator = include.comparator;

function add(a, b) {
  if (a.TAG === /* Coord_V3 */0) {
    if (b.TAG === /* Coord_V3 */0) {
      return {
              TAG: /* Coord_V3 */0,
              _0: Coord_V3$AdventOfCode.add(a._0, b._0)
            };
    } else {
      return Pervasives.failwith("Invalid addition");
    }
  } else if (b.TAG === /* Coord_V3 */0) {
    return Pervasives.failwith("Invalid addition");
  } else {
    return {
            TAG: /* Coord_V4 */1,
            _0: Coord_V4$AdventOfCode.add(a._0, b._0)
          };
  }
}

var Coord = {
  compare: compare,
  comparator: comparator,
  add: add
};

function makeGrid(lines) {
  var createActive = function (x, y) {
    if (Belt_Array.getExn(Belt_Array.getExn(lines, y), x) === "#") {
      return {
              TAG: /* Coord_V3 */0,
              _0: [
                x,
                y,
                0
              ]
            };
    }
    
  };
  var maxX = Belt_Array.getExn(lines, 0).length - 1 | 0;
  var maxY = lines.length - 1 | 0;
  var xs = Belt_Array.range(0, maxX);
  var ys = Belt_Array.range(0, maxY);
  return TableclothSet.fromArray(FP_Utils$AdventOfCode.combinationIfArray2(xs, ys, createActive), {
              comparator: comparator
            });
}

function conv34Cell(param) {
  if (param.TAG === /* Coord_V3 */0) {
    var match = param._0;
    return {
            TAG: /* Coord_V4 */1,
            _0: [
              match[0],
              match[1],
              match[2],
              0
            ]
          };
  }
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2020_Day17.res",
          53,
          17
        ],
        Error: new Error()
      };
}

function conv34(grid) {
  return TableclothSet.fromArray(TableclothArray.map(TableclothSet.toArray(grid), conv34Cell), {
              comparator: comparator
            });
}

function neighbourSpaces(here) {
  if (here.TAG === /* Coord_V3 */0) {
    return TableclothSet.fromArray(FP_Utils$AdventOfCode.combinationIfArray3([
                    -1,
                    0,
                    1
                  ], [
                    -1,
                    0,
                    1
                  ], [
                    -1,
                    0,
                    1
                  ], (function (x, y, z) {
                      if (x === 0 && y === 0 && z === 0) {
                        return ;
                      } else {
                        return add({
                                    TAG: /* Coord_V3 */0,
                                    _0: [
                                      x,
                                      y,
                                      z
                                    ]
                                  }, here);
                      }
                    })), {
                comparator: comparator
              });
  } else {
    return TableclothSet.fromArray(FP_Utils$AdventOfCode.combinationIfArray4([
                    -1,
                    0,
                    1
                  ], [
                    -1,
                    0,
                    1
                  ], [
                    -1,
                    0,
                    1
                  ], [
                    -1,
                    0,
                    1
                  ], (function (x, y, z, w) {
                      if (x === 0 && y === 0 && z === 0 && w === 0) {
                        return ;
                      } else {
                        return add({
                                    TAG: /* Coord_V4 */1,
                                    _0: [
                                      x,
                                      y,
                                      z,
                                      w
                                    ]
                                  }, here);
                      }
                    })), {
                comparator: comparator
              });
  }
}

function countOccupiedNeighbours(cell, grid) {
  return TableclothSet.length(TableclothSet.intersection(neighbourSpaces(cell), grid));
}

function cubeSurvives(grid, cell) {
  var alive = TableclothSet.includes(grid, cell);
  var nNbrs = countOccupiedNeighbours(cell, grid);
  if (alive) {
    if (nNbrs === 2) {
      return true;
    } else {
      return nNbrs === 3;
    }
  } else {
    return false;
  }
}

function cubeBorn(grid, cell) {
  var dead = !TableclothSet.includes(grid, cell);
  var nNbrs = countOccupiedNeighbours(cell, grid);
  if (dead) {
    return nNbrs === 3;
  } else {
    return false;
  }
}

function update(grid) {
  var mergeEmpties = function (acc, cell) {
    return TableclothSet.union(acc, neighbourSpaces(cell));
  };
  var empties = TableclothSet.difference(TableclothSet.fold(grid, TableclothSet.empty({
                comparator: comparator
              }), mergeEmpties), grid);
  return TableclothSet.union(TableclothSet.filter(grid, (function (param) {
                    return cubeSurvives(grid, param);
                  })), TableclothSet.filter(empties, (function (param) {
                    return cubeBorn(grid, param);
                  })));
}

function iterate(_grid, f, _times) {
  while(true) {
    var times = _times;
    var grid = _grid;
    if (times === 0) {
      return grid;
    }
    _times = times - 1 | 0;
    _grid = Curry._1(f, grid);
    continue ;
  };
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (param) {
                return FP_Utils$AdventOfCode.compose((function (prim) {
                              return prim.trim();
                            }), Utils$AdventOfCode.splitChars, param);
              }));
}

function solvePart1(data) {
  var grid0 = makeGrid(parse(data));
  return TableclothSet.length(iterate(grid0, update, 6));
}

function solvePart2(data) {
  var grid = conv34(makeGrid(parse(data)));
  return TableclothSet.length(iterate(grid, update, 6));
}

var TC;

export {
  log ,
  TC ,
  Coord ,
  makeGrid ,
  conv34Cell ,
  conv34 ,
  neighbourSpaces ,
  countOccupiedNeighbours ,
  cubeSurvives ,
  cubeBorn ,
  update ,
  iterate ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* include Not a pure module */
