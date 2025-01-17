// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Array2D from "../../Array2D.res.mjs";
import * as Coord_V2 from "../../Coord_V2.res.mjs";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.res.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.res.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function mkPipe(char) {
  switch (char) {
    case "-" :
      return "WE";
    case "7" :
      return "WS";
    case "F" :
      return "SE";
    case "J" :
      return "NW";
    case "L" :
      return "NE";
    case "S" :
      return "Start";
    case "|" :
      return "NS";
    default:
      return "Empty";
  }
}

function deltas(pipe) {
  let north = [
    0,
    -1
  ];
  let south = [
    0,
    1
  ];
  let east = [
    1,
    0
  ];
  let west = [
    -1,
    0
  ];
  switch (pipe) {
    case "Empty" :
      return [];
    case "NW" :
      return [
        north,
        west
      ];
    case "NS" :
      return [
        north,
        south
      ];
    case "NE" :
      return [
        north,
        east
      ];
    case "WE" :
      return [
        west,
        east
      ];
    case "WS" :
      return [
        west,
        south
      ];
    case "SE" :
      return [
        south,
        east
      ];
    case "Start" :
      return deltas("NS").concat(deltas("WE"));
  }
}

function isVertex(pipe) {
  switch (pipe) {
    case "Empty" :
    case "NS" :
    case "WE" :
      return false;
    default:
      return true;
  }
}

function findStart(grid) {
  return Array2D.reduceWithIndex(grid, [
    0,
    0
  ], (acc, elem, pos) => {
    if (elem === "Start") {
      return pos;
    } else {
      return acc;
    }
  });
}

function make(grid) {
  return {
    grid: grid,
    start: findStart(grid)
  };
}

let $$Map = {
  make: make
};

function neighbours(param, p) {
  let grid = param.grid;
  return Stdlib__Array.filterMap(deltas(Array2D.getExn(grid, p)), delta => {
    let nbr = Coord_V2.add(p, delta);
    if (Array2D.isValidXY(grid, nbr)) {
      return nbr;
    }
    
  });
}

function connectorsToPosition(map, pos) {
  let startNbrs = neighbours(map, pos);
  let nbrsNbrs = startNbrs.map(nbr => [
    nbr,
    neighbours(map, nbr)
  ]);
  let connectors = nbrsNbrs.filter(param => Stdlib__Option.isSome(param[1].find(n => Coord_V2.compare(n, pos) === 0)));
  return connectors.map(prim => prim[0]);
}

function connectorsToStart(map) {
  return connectorsToPosition(map, map.start);
}

function followPath(map, start) {
  let _acc = [];
  let _thisPos = start;
  let _lastPos = start;
  while (true) {
    let lastPos = _lastPos;
    let thisPos = _thisPos;
    let acc = _acc;
    let nbrs = connectorsToPosition(map, thisPos);
    let next = Stdlib__Array.getUnsafe(nbrs.filter(n => Coord_V2.compare(n, lastPos) !== 0), 0);
    let acc$p = acc.concat([thisPos]);
    if (Coord_V2.compare(next, start) === 0) {
      return acc$p;
    }
    _lastPos = thisPos;
    _thisPos = next;
    _acc = acc$p;
    continue;
  };
}

function part1(map) {
  let path = followPath(map, map.start);
  return path.length / 2 | 0;
}

function shoelaceFormula(v) {
  let v$p = Stdlib__Array.drop(v, 1).concat(Stdlib__Array.take(v, 1));
  return Math.abs(Stdlib__Array.sum(Stdlib__Array.map2(v, v$p, (param, param$1) => Math.imul(param[0], param$1[1]) - Math.imul(param[1], param$1[0]) | 0), {
    zero: Stdlib__Int.zero,
    add: Stdlib__Int.add
  })) / 2 | 0;
}

function part2(map) {
  let path = followPath(map, map.start);
  let boundaryPointsCount = path.length;
  let vertices = path.filter(pos => isVertex(Array2D.getExn(map.grid, pos)));
  let loopArea = shoelaceFormula(vertices);
  return (loopArea - (boundaryPointsCount / 2 | 0) | 0) + 1 | 0;
}

function parse(data) {
  return Utils.splitNewline(data).map(x => x.trim().split("").map(mkPipe));
}

function solvePart1(data) {
  let grid = parse(data);
  return part1({
    grid: grid,
    start: findStart(grid)
  });
}

function solvePart2(data) {
  let grid = parse(data);
  return part2({
    grid: grid,
    start: findStart(grid)
  });
}

export {
  log,
  log2,
  mkPipe,
  deltas,
  isVertex,
  findStart,
  $$Map,
  neighbours,
  connectorsToPosition,
  connectorsToStart,
  followPath,
  part1,
  shoelaceFormula,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */