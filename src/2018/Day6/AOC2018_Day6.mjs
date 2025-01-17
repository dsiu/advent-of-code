// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "rescript/lib/es6/js_int.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Caml_splice_call from "rescript/lib/es6/caml_splice_call.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as AOC2018_Day6_Data$AdventOfCode from "./AOC2018_Day6_Data.mjs";
import * as AOC2018_Day6_Data_Sample$AdventOfCode from "./AOC2018_Day6_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

function x(t) {
  return t.x;
}

function y(t) {
  return t.y;
}

function make(x, y) {
  return {
          x: x,
          y: y
        };
}

function makeFromArray(xs) {
  return {
          x: Belt_Array.getExn(xs, 0),
          y: Belt_Array.getExn(xs, 1)
        };
}

function findXY(f, init, xs) {
  return Belt_Array.reduce(xs, {
              x: init,
              y: init
            }, (function (a, c) {
                return {
                        x: f(a.x, c.x),
                        y: f(a.y, c.y)
                      };
              }));
}

function maxXY(param) {
  return findXY((function (prim0, prim1) {
                return Math.max(prim0, prim1);
              }), Js_int.min, param);
}

function minXY(param) {
  return findXY((function (prim0, prim1) {
                return Math.min(prim0, prim1);
              }), Js_int.max, param);
}

function dist(a, b) {
  return Math.abs(b.x - a.x | 0) + Math.abs(b.y - a.y | 0) | 0;
}

function parse(l) {
  return makeFromArray(Belt_Array.map(l.trim().split(","), Utils$AdventOfCode.intFromStringExn));
}

function parseCoords(__x) {
  return Belt_Array.map(__x, parse);
}

var Coord = {
  x: x,
  y: y,
  make: make,
  makeFromArray: makeFromArray,
  findXY: findXY,
  maxXY: maxXY,
  minXY: minXY,
  dist: dist,
  parse: parse,
  parseCoords: parseCoords
};

function w(t) {
  return t.w;
}

function h(t) {
  return t.h;
}

function grid(t) {
  return t.grid;
}

function distsFromPins(at, pins) {
  return Belt_MapInt.map(pins, (function (v) {
                return dist(at, v);
              }));
}

function findMinDists(__x) {
  return Belt_MapInt.reduce(__x, Js_int.max, (function (a, _k, v) {
                if (v < a) {
                  return v;
                } else {
                  return a;
                }
              }));
}

function keepOnly(value, xs) {
  return Belt_MapInt.keep(xs, (function (_k, v) {
                return v === value;
              }));
}

function makeCellShortest(at, pins) {
  var dists = distsFromPins(at, pins);
  var minDist = findMinDists(dists);
  var onlyMins = keepOnly(minDist, dists);
  if (Belt_MapInt.size(onlyMins) <= 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "AOC2018_Day6.res",
            93,
            4
          ],
          Error: new Error()
        };
  }
  if (Belt_MapInt.size(onlyMins) > 1) {
    return -1;
  } else {
    return Belt_MapInt.reduce(onlyMins, Js_int.min, (function (_a, k, _v) {
                  return k;
                }));
  }
}

function alloc(t) {
  var filled = Belt_Array.reduce(Belt_Array.range(0, t.w), undefined, (function (a, x) {
          return Belt_MapInt.set(a, x, Belt_Array.reduce(Belt_Array.range(0, t.h), undefined, (function (b, y) {
                            return Belt_MapInt.set(b, y, -99);
                          })));
        }));
  return {
          pins: t.pins,
          grid: filled,
          w: t.w,
          h: t.h,
          maxBound: t.maxBound,
          minBound: t.minBound
        };
}

function fill(t) {
  var filled = Belt_MapInt.reduce(t.grid, undefined, (function (a, kx, x) {
          return Belt_MapInt.set(a, kx, Belt_MapInt.reduce(x, undefined, (function (a, ky, _y) {
                            return Belt_MapInt.set(a, ky, makeCellShortest({
                                            x: kx,
                                            y: ky
                                          }, t.pins));
                          })));
        }));
  return {
          pins: t.pins,
          grid: filled,
          w: t.w,
          h: t.h,
          maxBound: t.maxBound,
          minBound: t.minBound
        };
}

function make$1(xs) {
  var maxBound = maxXY(xs);
  var minBound = minXY(xs);
  console.log(maxBound);
  console.log(minBound);
  var pinsMap = Belt_Array.reduceWithIndex(xs, undefined, (function (a, x, i) {
          return Belt_MapInt.set(a, i, x);
        }));
  var prim = Utils$AdventOfCode.Printable.MapInt.toString(pinsMap, (function (c) {
          return String(c.x) + " " + String(c.y);
        }));
  console.log(prim);
  return fill(alloc({
                  pins: pinsMap,
                  grid: undefined,
                  w: maxBound.x,
                  h: maxBound.y,
                  maxBound: maxBound,
                  minBound: minBound
                }));
}

function countCellWith(pinId, t) {
  return Belt_MapInt.reduce(t.grid, 0, (function (a, _kx, x) {
                return a + Belt_MapInt.size(keepOnly(pinId, x)) | 0;
              }));
}

function getNonInfPin(t) {
  var minBound = t.minBound;
  var maxBound = t.maxBound;
  return Belt_MapInt.keep(t.pins, (function (_k, v) {
                return !(v.x === maxBound.x || v.x === minBound.x || v.y === maxBound.y || v.y === minBound.y);
              }));
}

function findLandingAreasOfPins(t) {
  return Belt_MapInt.mapWithKey(t.pins, (function (k, _v) {
                return countCellWith(k, t);
              }));
}

function getMaxArea(m) {
  return Belt_MapInt.reduce(m, 0, (function (a, _k, v) {
                return Math.max(a, v);
              }));
}

function numToChar(xs) {
  return Belt_Array.map(xs, (function (x) {
                if (x !== -1) {
                  return String.fromCharCode(97 + x | 0);
                } else {
                  return ".";
                }
              }));
}

function toString(t) {
  console.log("x, y, v");
  Belt_MapInt.forEach(t.grid, (function (_kx, vx) {
          Caml_splice_call.spliceApply(console.log, [numToChar(Belt_MapInt.valuesToArray(vx))]);
        }));
}

var LandingMap = {
  w: w,
  h: h,
  grid: grid,
  distsFromPins: distsFromPins,
  findMinDists: findMinDists,
  keepOnly: keepOnly,
  makeCellShortest: makeCellShortest,
  alloc: alloc,
  fill: fill,
  make: make$1,
  countCellWith: countCellWith,
  getNonInfPin: getNonInfPin,
  findLandingAreasOfPins: findLandingAreasOfPins,
  getMaxArea: getMaxArea,
  numToChar: numToChar,
  toString: toString
};

function solvePart1(data) {
  var map = make$1(Belt_Array.map(data.split("\n"), parse));
  var areas = findLandingAreasOfPins(map);
  var prim = " ========= landing areas (size = " + String(Belt_MapInt.size(areas)) + ")";
  console.log(prim);
  var prim$1 = Utils$AdventOfCode.Printable.MapInt.Int.toString(areas, undefined);
  console.log(prim$1);
  var targetPins = getNonInfPin(map);
  var prim$2 = " ======== target pins (size = " + String(Belt_MapInt.size(targetPins)) + ")";
  console.log(prim$2);
  var prim$3 = Utils$AdventOfCode.Printable.MapInt.toString(targetPins, (function (c) {
          return String(c.x) + " " + String(c.y);
        }));
  console.log(prim$3);
  var maxArea = Belt_MapInt.reduce(Belt_MapInt.keep(areas, (function (k, _v) {
              return Belt_MapInt.has(targetPins, k);
            })), Js_int.min, (function (a, _k, v) {
          if (v > a) {
            return v;
          } else {
            return a;
          }
        }));
  console.log(" ======== answer");
  var prim$4 = "maxArea = " + String(maxArea);
  console.log(prim$4);
}

function solvePart2(data) {
  console.log(data);
  return 2;
}

var data = AOC2018_Day6_Data$AdventOfCode.data;

var sampleData = AOC2018_Day6_Data_Sample$AdventOfCode.data;

export {
  log ,
  data ,
  sampleData ,
  Coord ,
  LandingMap ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
