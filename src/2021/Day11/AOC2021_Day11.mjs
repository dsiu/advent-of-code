// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as FP_Utils$AdventOfCode from "../../FP_Utils.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function adjCoords(c) {
  return Belt_Array.map([
              Coordinate$AdventOfCode.stepNW,
              Coordinate$AdventOfCode.stepN,
              Coordinate$AdventOfCode.stepNE,
              Coordinate$AdventOfCode.stepW,
              Coordinate$AdventOfCode.stepE,
              Coordinate$AdventOfCode.stepSW,
              Coordinate$AdventOfCode.stepS,
              Coordinate$AdventOfCode.stepSE
            ], (function (f) {
                return f(c);
              }));
}

function getAdjacentCoords(t, c) {
  return Belt_Array.keepMap(adjCoords(c), (function (c) {
                if (Array2D$AdventOfCode.isValidXY(t, c)) {
                  return c;
                }
                
              }));
}

function getAdjacents(t, param) {
  return Belt_Array.keepMapU(adjCoords([
                  param[0],
                  param[1]
                ]), (function (c) {
                if (Array2D$AdventOfCode.isValidXY(t, c)) {
                  return Caml_option.some(Array2D$AdventOfCode.getExn(t, c));
                }
                
              }));
}

function count9Plus(t) {
  return Belt_Array.keep(t, (function (b) {
                return b >= 9;
              })).length;
}

function countZero(t) {
  return Belt_Array.keep(t, (function (b) {
                return b === 0;
              })).length;
}

function increaseEnergy(t) {
  return Array2D$AdventOfCode.mapU(t, (function (x) {
                return Utils$AdventOfCode.add(1, x);
              }));
}

function getFlashingCoords(t) {
  return Array2D$AdventOfCode.reduceWithIndex(t, [], (function (a, e, coord) {
                if (e > 9) {
                  return Belt_Array.concat(a, [coord]);
                } else {
                  return a;
                }
              }));
}

function performFlash(t, coord) {
  var neighbors = getAdjacentCoords(t, coord);
  Belt_Array.forEachU(neighbors, (function (n_addr) {
          var orig = Array2D$AdventOfCode.getExn(t, n_addr);
          if (Array2D$AdventOfCode.set(t, n_addr, orig > 0 ? orig + 1 | 0 : orig)) {
            return ;
          }
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
        }));
  return t;
}

function dim(t, coord) {
  if (Array2D$AdventOfCode.set(t, coord, 0)) {
    return t;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function iterate(t) {
  var next = increaseEnergy(t);
  while(true) {
    var flashings = getFlashingCoords(next);
    if (flashings.length === 0) {
      return next;
    }
    Belt_Array.forEachU(flashings, (function (flash_coord) {
            dim(performFlash(next, flash_coord), flash_coord);
          }));
    continue ;
  };
}

function iterateAndReduceN(_t, _n, _acc, reducer) {
  while(true) {
    var acc = _acc;
    var n = _n;
    var t = _t;
    var next = iterate(t);
    var acc$1 = Curry._2(reducer, acc, t);
    if ((n - 1 | 0) < 0) {
      return acc$1;
    }
    _acc = acc$1;
    _n = n - 1 | 0;
    _t = next;
    continue ;
  };
}

function countFlashN(t, n) {
  return iterateAndReduceN(t, n, 0, (function (acc, t) {
                return acc + countZero(Array2D$AdventOfCode.flatten(t)) | 0;
              }));
}

function iterateN(t, n) {
  return iterateAndReduceN(t, n, t, (function (param, t) {
                return t;
              }));
}

function flashesAtN(t, n) {
  return iterateAndReduceN(t, n, 0, (function (param, t) {
                return countZero(Array2D$AdventOfCode.flatten(t));
              }));
}

function toString(t) {
  var ret = [];
  for(var i = 0 ,i_finish = Array2D$AdventOfCode.lengthY(t); i < i_finish; ++i){
    var row = Belt_Option.getWithDefault(Array2D$AdventOfCode.getYEquals(t, i), []);
    ret = Belt_Array.concat(ret, [Belt_Array.map(row, (function (x) {
                    return x.toString();
                  })).join("")]);
  }
  return ret.join("\n");
}

var Octopus = {
  adjCoords: adjCoords,
  getAdjacentCoords: getAdjacentCoords,
  getAdjacents: getAdjacents,
  count9Plus: count9Plus,
  countZero: countZero,
  increaseEnergy: increaseEnergy,
  getFlashingCoords: getFlashingCoords,
  performFlash: performFlash,
  dim: dim,
  iterate: iterate,
  iterateAndReduceN: iterateAndReduceN,
  countFlashN: countFlashN,
  iterateN: iterateN,
  flashesAtN: flashesAtN,
  toString: toString
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (param) {
                return FP_Utils$AdventOfCode.compose((function (prim) {
                              return prim.trim();
                            }), (function (x) {
                              return Belt_Array.map(Utils$AdventOfCode.splitChars(x), Utils$AdventOfCode.intFromStringExn);
                            }), param);
              }));
}

function solvePart1_try(data) {
  console.log("orig ----");
  var d = parse(data);
  console.log(toString(d));
  console.log("orig ----");
  console.log("iterate " + String(1) + " ----");
  var e = iterateN(d, 1);
  console.log(toString(e));
  console.log("iterate " + String(1) + " ----");
  console.log("iterate " + String(100) + " ----");
  var e$1 = iterateN(d, 100);
  console.log(toString(e$1));
  console.log("iterate " + String(100) + " ----");
}

function solvePart1(data) {
  var d = parse(data);
  return countFlashN(d, 100);
}

function solvePart2(data) {
  var d = parse(data);
  var i = 0;
  var c = 0;
  while(c < 100) {
    c = flashesAtN(d, i);
    i = i + 1 | 0;
  };
  return i - 1 | 0;
}

export {
  log ,
  Octopus ,
  parse ,
  solvePart1_try ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
