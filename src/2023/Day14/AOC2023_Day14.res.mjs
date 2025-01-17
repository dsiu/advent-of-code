// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.res.mjs";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.res.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function readElement(str) {
  switch (str) {
    case "#" :
      return "Cube";
    case "." :
      return "Empty";
    case "O" :
      return "Round";
    default:
      return Pervasives.failwith("Invalid element");
  }
}

function makeGrid(xss) {
  return Stdlib__Array.transpose(xss.map(__x => __x.map(readElement)));
}

function showElement(el) {
  switch (el) {
    case "Empty" :
      return ".";
    case "Cube" :
      return "#";
    case "Round" :
      return "O";
  }
}

function showGrid(grid) {
  return Stdlib__Array.transpose(grid).map(row => row.map(showElement).join("")).join("\n");
}

function gridEq(g1, g2) {
  return showGrid(g1) === showGrid(g2);
}

function rollStep(param, source) {
  let target = param[1];
  let handled = param[0];
  switch (target) {
    case "Empty" :
      switch (source) {
        case "Empty" :
        case "Cube" :
          break;
        case "Round" :
          return [
            handled.concat(["Round"]),
            "Empty"
          ];
      }
      break;
    case "Cube" :
    case "Round" :
      break;
  }
  return [
    handled.concat([target]),
    source
  ];
}

function roll(xs) {
  if (xs.length === 0) {
    return [];
  }
  let l = Stdlib__Array.headUnsafe(xs);
  let ls = Stdlib__Array.tail(xs);
  let match = Stdlib__Array.fold(ls, [
    [],
    l
  ], rollStep);
  return match[0].concat([match[1]]);
}

function rollGrid(__x) {
  return __x.map(roll);
}

function rollToCompletion(_grid) {
  while (true) {
    let grid = _grid;
    let grid$p = grid.map(roll);
    if (gridEq(grid, grid$p)) {
      return grid$p;
    }
    _grid = grid$p;
    continue;
  };
}

function scoreGrid(grid) {
  let normalizedGrid = Stdlib__Array.transpose(grid);
  let l = normalizedGrid.length;
  let indexedGird = Stdlib__Array.zip(Stdlib__Array.fromInitializer(l, i => i + 1 | 0), normalizedGrid.toReversed());
  let scoreRow = param => Math.imul(param[0], Stdlib__Array.count(param[1], el => el === "Round"));
  return Stdlib__Array.sum(indexedGird.map(scoreRow), {
    zero: Stdlib__Int.zero,
    add: Stdlib__Int.add
  });
}

function rotate1(extra) {
  return Stdlib__Function.compose(__x => __x.map(prim => prim.toReversed()), Stdlib__Array.transpose, extra);
}

function runNTimes(_n, f, _x) {
  while (true) {
    let x = _x;
    let n = _n;
    if (n === 0) {
      return x;
    }
    _x = f(x);
    _n = n - 1 | 0;
    continue;
  };
}

function runNTimesWithCache(_n, f, _x, cacheFn, predFn) {
  while (true) {
    let x = _x;
    let n = _n;
    if (n === 0) {
      return [
        0,
        x
      ];
    }
    let x$p = f(x);
    if (predFn(x$p)) {
      console.log("done. cycles = ", n);
      return [
        n,
        x$p
      ];
    }
    cacheFn(x$p, n);
    _x = x$p;
    _n = n - 1 | 0;
    continue;
  };
}

function rollCycle(grid) {
  let oneTurn = grid => rotate1(rollToCompletion(grid));
  return runNTimes(4, oneTurn, grid);
}

function part1(grid) {
  return scoreGrid(rollToCompletion(grid));
}

function part2(grid) {
  let cache = new Map();
  let predFn = grid => cache.has(showGrid(grid));
  let cacheFn = (grid, i) => {
    cache.set(showGrid(grid), [
      grid,
      1000000000 - i | 0
    ]);
  };
  let match = runNTimesWithCache(1000000000, rollCycle, grid, cacheFn, predFn);
  let repeatEnd = 1000000000 - match[0] | 0;
  let firstSeen = cache.get(showGrid(match[1]));
  let tmp;
  if (firstSeen !== undefined) {
    let repeatStart = firstSeen[1];
    let repeatLen = repeatEnd - repeatStart | 0;
    let finalIndex = Primitive_int.mod_(1000000000 - repeatStart | 0, repeatLen) + repeatStart | 0;
    let ret = Array.from(cache.entries()).filter(param => param[1][1] === (finalIndex - 1 | 0));
    let match$1 = ret[0];
    if (match$1 !== undefined) {
      tmp = match$1[1][0];
    } else {
      throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2023_Day14.res",
          146,
          10
        ],
        Error: new Error()
      };
    }
  } else {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  return scoreGrid(tmp);
}

function trimAndSplit(str) {
  return str.trim().split("");
}

function parse(data) {
  return Utils.splitNewline(data).map(trimAndSplit);
}

function solvePart1(data) {
  let grid = makeGrid(Utils.splitNewline(data).map(trimAndSplit));
  return scoreGrid(rollToCompletion(grid));
}

function solvePart2(data) {
  return part2(makeGrid(Utils.splitNewline(data).map(trimAndSplit)));
}

export {
  log,
  log2,
  readElement,
  makeGrid,
  showElement,
  showGrid,
  gridEq,
  rollStep,
  roll,
  rollGrid,
  rollToCompletion,
  scoreGrid,
  rotate1,
  runNTimes,
  runNTimesWithCache,
  rollCycle,
  part1,
  part2,
  trimAndSplit,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */