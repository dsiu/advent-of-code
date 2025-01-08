// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.mjs";
import * as Belt_Map from "rescript/lib/es6/Belt_Map.js";
import * as Coord_V2 from "../../Coord_V2.mjs";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as TableclothMap from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothMap.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as TableclothList from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothList.mjs";
import * as TableclothArray from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothArray.mjs";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function manhattan(param) {
  return Math.abs(param[0]) + Math.abs(param[1]) | 0;
}

let emptyVisited = TableclothMap.empty({
  comparator: Coord_V2.comparator
});

let InvalidDirection = /* @__PURE__ */Primitive_exceptions.create("AOC2019_Day3.InvalidDirection");

function makeSegment(str) {
  let direction = str[0];
  let steps = Stdlib__Option.getExn(Stdlib__Int.fromString(str.slice(1), undefined), undefined);
  if (direction !== undefined) {
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
          Error: new Error()
        };
    }
  } else {
    throw {
      RE_EXN_ID: InvalidDirection,
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
  let delta = facing(segment.direction);
  let distance = segment.steps;
  let start = path.tip;
  let visited = path.visited;
  let len = path.currentLength;
  let len$p = len + distance | 0;
  let insertStep = (visits, param) => {
    let loc = param[1];
    if (TableclothMap.includes(visits, loc)) {
      return visits;
    } else {
      return TableclothMap.add(visits, loc, param[0]);
    }
  };
  let visited$p = TableclothList.fold(Stdlib__List.zip(TableclothList.initialize(distance, x => (x + len | 0) + 1 | 0), Stdlib__List.unfold(param => param[0] >= distance, param => {
    let x = param[1];
    return [
      Coord_V2.add(x, delta),
      [
        param[0] + 1 | 0,
        Coord_V2.add(x, delta)
      ]
    ];
  }, [
    0,
    start
  ])), visited, insertStep);
  let tip$p = Coord_V2.add(start, Coord_V2.mul(delta, distance));
  return {
    visited: visited$p,
    tip: tip$p,
    currentLength: len$p
  };
}

function travelPath(segments) {
  let path0_tip = [
    0,
    0
  ];
  let path0 = {
    visited: emptyVisited,
    tip: path0_tip,
    currentLength: 0
  };
  return TableclothArray.fold(segments, path0, travelSegment);
}

function travelAllPaths(__x) {
  return __x.map(travelPath);
}

function closest(points) {
  return Utils.minIntInArray(Belt_Map.keysToArray(points).map(manhattan));
}

function crossovers(travelledPaths) {
  return Stdlib__Array.foldl1(travelledPaths.map(param => param.visited), (m1, m2) => TableclothMap.merge(m1, m2, (_k, a, b) => {
    if (a !== undefined && b !== undefined) {
      return a + b | 0;
    }
    
  }));
}

function shortestPaths(crossings) {
  return Utils.minIntInArray(Belt_Map.valuesToArray(crossings));
}

function part1(segmentss) {
  return closest(crossovers(segmentss.map(travelPath)));
}

function part2(segmentss) {
  let crossings = crossovers(segmentss.map(travelPath));
  return Utils.minIntInArray(Belt_Map.valuesToArray(crossings));
}

function parse(data) {
  return Utils.splitNewline(data).map(x => x.trim().split(","));
}

function solvePart1(data) {
  let segmentss = parse(data).map(__x => __x.map(makeSegment));
  return closest(crossovers(segmentss.map(travelPath)));
}

function solvePart2(data) {
  let segmentss = parse(data).map(__x => __x.map(makeSegment));
  let crossings = crossovers(segmentss.map(travelPath));
  return Utils.minIntInArray(Belt_Map.valuesToArray(crossings));
}

let TC;

let Location;

export {
  log,
  log2,
  TC,
  Location,
  manhattan,
  emptyVisited,
  InvalidDirection,
  makeSegment,
  facing,
  travelSegment,
  travelPath,
  travelAllPaths,
  closest,
  crossovers,
  shortestPaths,
  part1,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* emptyVisited Not a pure module */
