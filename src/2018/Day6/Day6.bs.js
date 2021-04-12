// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("@rescript/std/lib/js/curry.js");
var Js_int = require("@rescript/std/lib/js/js_int.js");
var Belt_Int = require("@rescript/std/lib/js/belt_Int.js");
var Belt_Array = require("@rescript/std/lib/js/belt_Array.js");
var Belt_MapInt = require("@rescript/std/lib/js/belt_MapInt.js");
var Belt_Option = require("@rescript/std/lib/js/belt_Option.js");
var Utils$AdventOfCode = require("../Utils.bs.js");
var Day6_Data$AdventOfCode = require("./Day6_Data.bs.js");
var Day6_Data_Test$AdventOfCode = require("./Day6_Data_Test.bs.js");

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
          x: Belt_Option.getExn(Belt_Array.get(xs, 0)),
          y: Belt_Option.getExn(Belt_Array.get(xs, 1))
        };
}

function findXY(f, init, xs) {
  return Belt_Array.reduce(xs, {
              x: init,
              y: init
            }, (function (a, c) {
                return {
                        x: Curry._2(f, a.x, c.x),
                        y: Curry._2(f, a.y, c.y)
                      };
              }));
}

function maxXY(param) {
  return findXY((function (prim, prim$1) {
                return Math.max(prim, prim$1);
              }), Js_int.min, param);
}

function minXY(param) {
  return findXY((function (prim, prim$1) {
                return Math.min(prim, prim$1);
              }), Js_int.max, param);
}

function dist(a, b) {
  return Math.abs(b.x - a.x | 0) + Math.abs(b.y - a.y | 0) | 0;
}

function parse(l) {
  return makeFromArray(Belt_Array.map(l.trim().split(","), (function (x) {
                    return Belt_Option.getExn(Belt_Int.fromString(x.trim()));
                  })));
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

function distsFromLocs(at, pins) {
  return Belt_MapInt.reduce(pins, undefined, (function (a, k, v) {
                return Belt_MapInt.set(a, k, dist(at, v));
              }));
}

function findMinDists(__x) {
  return Belt_MapInt.reduce(__x, Js_int.max, (function (a, k, v) {
                if (v < a) {
                  return v;
                } else {
                  return a;
                }
              }));
}

function keepOnly(value, xs) {
  return Belt_MapInt.keep(xs, (function (k, v) {
                return v === value;
              }));
}

function makeCellShortest(at, pins) {
  console.log("makeCellShortest");
  var dists = distsFromLocs(at, pins);
  console.log(" ");
  console.log("dists --> ");
  var minDist = findMinDists(dists);
  var prim = "at " + String(at.x) + "," + String(at.y) + " | minDist:" + String(minDist);
  console.log(prim);
  var onlyMins = keepOnly(minDist, dists);
  console.log("onlyMins -->");
  Utils$AdventOfCode.dump_mapInt_of_int(onlyMins);
  if (Belt_MapInt.size(onlyMins) <= 0) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "Day6.res",
            90,
            4
          ],
          Error: new Error()
        };
  }
  if (Belt_MapInt.size(onlyMins) > 1) {
    return -1;
  } else {
    return Belt_MapInt.reduce(onlyMins, Js_int.min, (function (a, k, v) {
                  return k;
                }));
  }
}

function alloc(t) {
  var filled = Belt_Array.reduce(Belt_Array.range(0, t.w - 1 | 0), undefined, (function (a, x) {
          return Belt_MapInt.set(a, x, Belt_Array.reduce(Belt_Array.range(0, t.h - 1 | 0), undefined, (function (b, y) {
                            return Belt_MapInt.set(b, y, -1);
                          })));
        }));
  return {
          pins: t.pins,
          grid: filled,
          w: t.w,
          h: t.h,
          maxXY: t.maxXY,
          minXY: t.minXY
        };
}

function fill(t) {
  var filled = Belt_MapInt.reduce(t.grid, undefined, (function (a, kx, x) {
          return Belt_MapInt.set(a, kx, Belt_MapInt.reduce(x, undefined, (function (a, ky, y) {
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
          maxXY: t.maxXY,
          minXY: t.minXY
        };
}

function make$1(xs) {
  var maxXY$1 = maxXY(xs);
  var minXY$1 = minXY(xs);
  console.log(maxXY$1);
  console.log(minXY$1);
  var pinsMap = Belt_Array.reduceWithIndex(xs, undefined, (function (a, x, i) {
          return Belt_MapInt.set(a, i, x);
        }));
  Utils$AdventOfCode.dump_mapInt_of((function (c) {
          return String(c.x) + " " + String(c.y);
        }), pinsMap);
  return fill(alloc({
                  pins: pinsMap,
                  grid: undefined,
                  w: maxXY$1.x + 1 | 0,
                  h: maxXY$1.y + 1 | 0,
                  maxXY: maxXY$1,
                  minXY: minXY$1
                }));
}

function countCellWith(value, t) {
  return Belt_MapInt.reduce(t.grid, 0, (function (a, kx, x) {
                return a + Belt_MapInt.size(keepOnly(value, x)) | 0;
              }));
}

function getNonInfLoc(t) {
  var minXY = t.minXY;
  var maxXY = t.maxXY;
  console.log("getNonInfLoc");
  console.log(t);
  return Belt_MapInt.keep(t.pins, (function (k, v) {
                console.log(v);
                return !(v.x === maxXY.x || v.x === minXY.x || v.y === minXY.y || v.y === minXY.y);
              }));
}

function findAreas(t) {
  return Belt_MapInt.mapWithKey(t.pins, (function (k, v) {
                return countCellWith(k, t);
              }));
}

function getMaxArea(m) {
  return Belt_MapInt.reduce(m, 0, (function (a, k, v) {
                return Math.max(a, v);
              }));
}

function dump(t) {
  console.log("dump");
  console.log("x, y, v");
  return Belt_MapInt.forEach(t.grid, (function (kx, vx) {
                return Belt_MapInt.forEach(vx, (function (ky, vy) {
                              console.log([
                                    kx,
                                    ky,
                                    vy
                                  ]);
                              
                            }));
              }));
}

var $$Map = {
  w: w,
  h: h,
  grid: grid,
  distsFromLocs: distsFromLocs,
  findMinDists: findMinDists,
  keepOnly: keepOnly,
  makeCellShortest: makeCellShortest,
  alloc: alloc,
  fill: fill,
  make: make$1,
  countCellWith: countCellWith,
  getNonInfLoc: getNonInfLoc,
  findAreas: findAreas,
  getMaxArea: getMaxArea,
  dump: dump
};

var data = Day6_Data$AdventOfCode.data;

var testData = Day6_Data_Test$AdventOfCode.data;

exports.log = log;
exports.data = data;
exports.testData = testData;
exports.Coord = Coord;
exports.$$Map = $$Map;
/* No side effect */
