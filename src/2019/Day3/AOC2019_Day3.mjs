// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Set from "rescript/lib/es6/belt_Set.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as TableclothSet from "tablecloth-rescript/src/TableclothSet.mjs";
import * as TableclothList from "tablecloth-rescript/src/TableclothList.mjs";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as TableclothArray from "tablecloth-rescript/src/TableclothArray.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Coord_V2$AdventOfCode from "../../Coord_V2.mjs";
import * as FP_Utils$AdventOfCode from "../../FP_Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function manhattan(param) {
  return Math.abs(param[0]) + Math.abs(param[1]) | 0;
}

var emptyVisited = TableclothSet.empty({
      comparator: Coord_V2$AdventOfCode.comparator
    });

var InvalidDirection = /* @__PURE__ */Caml_exceptions.create("AOC2019_Day3-AdventOfCode.InvalidDirection");

function makeSegment(str) {
  var direction = str[0];
  var steps = Belt_Option.getExn(Belt_Int.fromString(str.slice(1)));
  switch (direction) {
    case "D" :
        return {
                direction: /* South */1,
                steps: steps
              };
    case "L" :
        return {
                direction: /* West */2,
                steps: steps
              };
    case "R" :
        return {
                direction: /* East */0,
                steps: steps
              };
    case "U" :
        return {
                direction: /* North */3,
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
    case /* East */0 :
        return [
                1,
                0
              ];
    case /* South */1 :
        return [
                0,
                -1
              ];
    case /* West */2 :
        return [
                -1,
                0
              ];
    case /* North */3 :
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
  var visited$p = TableclothList.fold(FP_Utils$AdventOfCode.unfold((function (param) {
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
          ]), visited, TableclothSet.add);
  var tip$p = Coord_V2$AdventOfCode.add(start, Coord_V2$AdventOfCode.mul(delta, distance));
  return {
          visited: visited$p,
          tip: tip$p
        };
}

function travelPath(segments) {
  var path0_tip = [
    0,
    0
  ];
  var path0 = {
    visited: emptyVisited,
    tip: path0_tip
  };
  return TableclothArray.fold(segments, path0, travelSegment);
}

function travelAllPaths(__x) {
  return Belt_Array.map(__x, travelPath);
}

function closest(points) {
  return Utils$AdventOfCode.minIntInArray(Belt_Array.map(Belt_Set.toArray(points), manhattan));
}

function crossovers(travelledPaths) {
  return FP_Utils$AdventOfCode.foldLeftArray(Belt_Array.map(travelledPaths, (function (param) {
                    return param.visited;
                  })), TableclothSet.intersection);
}

function part1(segmentss) {
  return closest(crossovers(Belt_Array.map(segmentss, travelPath)));
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
  return 2;
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
  part1 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* emptyVisited Not a pure module */
