// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Map from "rescript/lib/es6/belt_Map.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Stdlib_List from "@dsiu/rescript-stdlib-fp/src/Stdlib_List.mjs";
import * as Stdlib_Array from "@dsiu/rescript-stdlib-fp/src/Stdlib_Array.mjs";
import * as TableclothMap from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothMap.mjs";
import * as TableclothList from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothList.mjs";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as TableclothArray from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothArray.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Coord_V2$AdventOfCode from "../../Coord_V2.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function manhattan(param) {
  return Math.abs(param[0]) + Math.abs(param[1]) | 0;
}

var emptyVisited = TableclothMap.empty({
      comparator: Coord_V2$AdventOfCode.comparator
    });

var InvalidDirection = /* @__PURE__ */Caml_exceptions.create("AOC2019_Day3-AdventOfCode.InvalidDirection");

function makeSegment(str) {
  var direction = str[0];
  var steps = Belt_Option.getExn(Belt_Int.fromString(str.slice(1)));
  switch (direction) {
    case "D" :
        return {
                direction: "South",
                steps: steps
              };
    case "L" :
        return {
                direction: "West",
                steps: steps
              };
    case "R" :
        return {
                direction: "East",
                steps: steps
              };
    case "U" :
        return {
                direction: "North",
                steps: steps
              };
    default:
      throw {
            RE_EXN_ID: InvalidDirection,
            _1: direction,
            Error: new Error()
          };
  }
}

function facing(direction) {
  switch (direction) {
    case "East" :
        return [
                1,
                0
              ];
    case "South" :
        return [
                0,
                -1
              ];
    case "West" :
        return [
                -1,
                0
              ];
    case "North" :
        return [
                0,
                1
              ];
    
  }
}

function travelSegment(path, segment) {
  var delta = facing(segment.direction);
  var distance = segment.steps;
  var start = path.tip;
  var visited = path.visited;
  var len = path.currentLength;
  var len$p = len + distance | 0;
  var insertStep = function (visits, param) {
    var loc = param[1];
    if (TableclothMap.includes(visits, loc)) {
      return visits;
    } else {
      return TableclothMap.add(visits, loc, param[0]);
    }
  };
  var visited$p = TableclothList.fold(TableclothList.zip(TableclothList.initialize(distance, (function (x) {
                  return (x + len | 0) + 1 | 0;
                })), Stdlib_List.unfold((function (param) {
                  return param[0] >= distance;
                }), (function (param) {
                  var x = param[1];
                  return [
                          Coord_V2$AdventOfCode.add(x, delta),
                          [
                            param[0] + 1 | 0,
                            Coord_V2$AdventOfCode.add(x, delta)
                          ]
                        ];
                }), [
                0,
                start
              ])), visited, insertStep);
  var tip$p = Coord_V2$AdventOfCode.add(start, Coord_V2$AdventOfCode.mul(delta, distance));
  return {
          visited: visited$p,
          tip: tip$p,
          currentLength: len$p
        };
}

function travelPath(segments) {
  var path0_tip = [
    0,
    0
  ];
  var path0 = {
    visited: emptyVisited,
    tip: path0_tip,
    currentLength: 0
  };
  return TableclothArray.fold(segments, path0, travelSegment);
}

function travelAllPaths(__x) {
  return Belt_Array.map(__x, travelPath);
}

function closest(points) {
  return Utils$AdventOfCode.minIntInArray(Belt_Array.map(Belt_Map.keysToArray(points), manhattan));
}

function crossovers(travelledPaths) {
  return Stdlib_Array.foldl1(Belt_Array.map(travelledPaths, (function (param) {
                    return param.visited;
                  })), (function (m1, m2) {
                return TableclothMap.merge(m1, m2, (function (_k, a, b) {
                              if (a !== undefined && b !== undefined) {
                                return a + b | 0;
                              }
                              
                            }));
              }));
}

function shortestPaths(crossings) {
  return Utils$AdventOfCode.minIntInArray(Belt_Map.valuesToArray(crossings));
}

function part1(segmentss) {
  return closest(crossovers(Belt_Array.map(segmentss, travelPath)));
}

function part2(segmentss) {
  var crossings = crossovers(Belt_Array.map(segmentss, travelPath));
  return Utils$AdventOfCode.minIntInArray(Belt_Map.valuesToArray(crossings));
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return x.trim().split(",");
              }));
}

function solvePart1(data) {
  var segmentss = Belt_Array.map(parse(data), (function (__x) {
          return Belt_Array.map(__x, makeSegment);
        }));
  return closest(crossovers(Belt_Array.map(segmentss, travelPath)));
}

function solvePart2(data) {
  var segmentss = Belt_Array.map(parse(data), (function (__x) {
          return Belt_Array.map(__x, makeSegment);
        }));
  var crossings = crossovers(Belt_Array.map(segmentss, travelPath));
  return Utils$AdventOfCode.minIntInArray(Belt_Map.valuesToArray(crossings));
}

var TC;

var $$Location;

export {
  log ,
  log2 ,
  TC ,
  $$Location ,
  manhattan ,
  emptyVisited ,
  InvalidDirection ,
  makeSegment ,
  facing ,
  travelSegment ,
  travelPath ,
  travelAllPaths ,
  closest ,
  crossovers ,
  shortestPaths ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* emptyVisited Not a pure module */
