// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Set from "rescript/lib/es6/belt_Set.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Belt_SortArrayInt from "rescript/lib/es6/belt_SortArrayInt.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function adjCoords(c) {
  return Belt_List.map({
              hd: Coordinate$AdventOfCode.StepFunctions.stepN,
              tl: {
                hd: Coordinate$AdventOfCode.StepFunctions.stepW,
                tl: {
                  hd: Coordinate$AdventOfCode.StepFunctions.stepE,
                  tl: {
                    hd: Coordinate$AdventOfCode.StepFunctions.stepS,
                    tl: /* [] */0
                  }
                }
              }
            }, (function (f) {
                return f(c);
              }));
}

function getAdjacents(t, param) {
  return Belt_List.keepMap(adjCoords([
                  param[0],
                  param[1]
                ]), (function (c) {
                if (Array2D$AdventOfCode.isValidXY(t, c)) {
                  return {
                          TAG: "CoordAndVal",
                          _0: c,
                          _1: Array2D$AdventOfCode.getExn(t, c)
                        };
                }
                
              }));
}

function isLowest(x, _adjs) {
  while(true) {
    var adjs = _adjs;
    if (!adjs) {
      return true;
    }
    if (x >= adjs.hd._1) {
      return false;
    }
    _adjs = adjs.tl;
    continue ;
  };
}

function getLowPoints(t) {
  return Array2D$AdventOfCode.reduceWithIndex(t, [], (function (a, p, param) {
                var y = param[1];
                var x = param[0];
                if (isLowest(p, getAdjacents(t, [
                            x,
                            y
                          ]))) {
                  return Belt_Array.concat(a, [{
                                TAG: "CoordAndVal",
                                _0: [
                                  x,
                                  y
                                ],
                                _1: p
                              }]);
                } else {
                  return a;
                }
              }));
}

function getAllPoints(t) {
  return Array2D$AdventOfCode.reduceWithIndex(t, [], (function (a, p, param) {
                return Belt_Array.concat(a, [{
                              TAG: "CoordAndVal",
                              _0: [
                                param[0],
                                param[1]
                              ],
                              _1: p
                            }]);
              }));
}

function cmp(param, param$1) {
  var c = Caml_obj.compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Caml_obj.compare(param[1], param$1[1]);
  }
}

var PairComparator = Belt_Id.MakeComparable({
      cmp: cmp
    });

function getBasin(t, param) {
  var helper = function (t, param, visited) {
    var y = param[1];
    var x = param[0];
    var v = Array2D$AdventOfCode.getExn(t, [
          x,
          y
        ]);
    if (v < 0) {
      return visited;
    }
    var new_set = Belt_Set.add(visited, [
          x,
          y
        ]);
    Array2D$AdventOfCode.set(t, [
          x,
          y
        ], -1);
    var candidates = Belt_List.keep(getAdjacents(t, [
              x,
              y
            ]), (function (param) {
            if (param._1 !== 9 && v >= 0) {
              return !Belt_Set.has(new_set, param._0);
            } else {
              return false;
            }
          }));
    return Belt_List.reduce(candidates, new_set, (function (acc, param) {
                  var match = param._0;
                  return Belt_Set.union(acc, helper(t, [
                                  match[0],
                                  match[1]
                                ], acc));
                }));
  };
  var b_set = Belt_Set.make(PairComparator);
  return helper(t, [
              param[0],
              param[1]
            ], b_set);
}

function getBasinSize(t, param) {
  return Belt_Set.size(getBasin(t, [
                  param[0],
                  param[1]
                ]));
}

function search(grid, x, y) {
  var row = Belt_Option.getWithDefault(Belt_Array.get(grid, y), []);
  var height = Belt_Option.getWithDefault(Belt_Array.get(row, x), 10);
  if (height >= 9 || height < 0) {
    return 0;
  } else {
    Belt_Array.set(row, x, -1);
    return (((1 + search(grid, x - 1 | 0, y) | 0) + search(grid, x + 1 | 0, y) | 0) + search(grid, x, y - 1 | 0) | 0) + search(grid, x, y + 1 | 0) | 0;
  }
}

function make(xs) {
  var x = Belt_Array.getExn(xs, 0).length;
  var y = xs.length;
  var ret = Array2D$AdventOfCode.make([
        x,
        y
      ], 0);
  Belt_Array.forEachWithIndex(xs, (function (y, ys) {
          Belt_Array.forEachWithIndex(ys, (function (x, c) {
                  Array2D$AdventOfCode.set(ret, [
                        x,
                        y
                      ], c);
                }));
        }));
  return ret;
}

var HeightMap = {
  adjCoords: adjCoords,
  getAdjacents: getAdjacents,
  isLowest: isLowest,
  getLowPoints: getLowPoints,
  getAllPoints: getAllPoints,
  PairComparator: PairComparator,
  getBasin: getBasin,
  getBasinSize: getBasinSize,
  search: search,
  make: make
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (extra) {
                return Stdlib__Function.compose((function (prim) {
                              return prim.trim();
                            }), (function (x) {
                              return Belt_Array.map(Utils$AdventOfCode.splitChars(x), Utils$AdventOfCode.intFromStringExn);
                            }), extra);
              }));
}

function solvePart1(data) {
  var hmap = make(parse(data));
  return Belt_Array.reduce(Belt_Array.map(getLowPoints(hmap), (function (param) {
                    return Utils$AdventOfCode.add(param._1, 1);
                  })), 0, (function (a, x) {
                return Utils$AdventOfCode.add(a, x);
              }));
}

function solvePart2(data) {
  var hmap = make(parse(data));
  var basins = Belt_Array.reverse(Belt_SortArrayInt.stableSort(Belt_Array.map(getLowPoints(hmap), (function (param) {
                  return getBasinSize(hmap, param._0);
                }))));
  var largest3 = Belt_Array.slice(basins, 0, 3);
  return Belt_Array.reduce(largest3, 1, Utils$AdventOfCode.mul);
}

function solvePart2_from_github(data) {
  var hmap = make(parse(data));
  var basins = Belt_Array.reverse(Belt_SortArrayInt.stableSort(Belt_Array.map(getLowPoints(hmap), (function (param) {
                  var match = param._0;
                  return search(hmap, match[0], match[1]);
                }))));
  var largest3 = Belt_Array.slice(basins, 0, 3);
  return Belt_Array.reduce(largest3, 1, Utils$AdventOfCode.mul);
}

export {
  log ,
  HeightMap ,
  parse ,
  solvePart1 ,
  solvePart2 ,
  solvePart2_from_github ,
}
/* PairComparator Not a pure module */
