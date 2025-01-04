// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function adjCoords(c) {
  return Belt_Array.map([
    Coordinate$AdventOfCode.StepFunctions.stepNW,
    Coordinate$AdventOfCode.StepFunctions.stepN,
    Coordinate$AdventOfCode.StepFunctions.stepNE,
    Coordinate$AdventOfCode.StepFunctions.stepW,
    Coordinate$AdventOfCode.StepFunctions.stepE,
    Coordinate$AdventOfCode.StepFunctions.stepSW,
    Coordinate$AdventOfCode.StepFunctions.stepS,
    Coordinate$AdventOfCode.StepFunctions.stepSE
  ], f => f(c));
}

function getAdjacentCoords(t, c) {
  return Belt_Array.keepMap(adjCoords(c), c => {
    if (Array2D$AdventOfCode.isValidXY(t, c)) {
      return c;
    }
    
  });
}

function getAdjacents(t, param) {
  return Belt_Array.keepMap(adjCoords([
    param[0],
    param[1]
  ]), c => {
    if (Array2D$AdventOfCode.isValidXY(t, c)) {
      return Primitive_option.some(Array2D$AdventOfCode.getExn(t, c));
    }
    
  });
}

function count9Plus(t) {
  return Belt_Array.keep(t, b => b >= 9).length;
}

function countZero(t) {
  return Belt_Array.keep(t, b => b === 0).length;
}

function increaseEnergy(t) {
  return Array2D$AdventOfCode.map(t, x => Utils$AdventOfCode.add(1, x));
}

function getFlashingCoords(t) {
  return Array2D$AdventOfCode.reduceWithIndex(t, [], (a, e, coord) => {
    if (e > 9) {
      return Belt_Array.concat(a, [coord]);
    } else {
      return a;
    }
  });
}

function performFlash(t, coord) {
  let neighbors = getAdjacentCoords(t, coord);
  Belt_Array.forEach(neighbors, n_addr => {
    let orig = Array2D$AdventOfCode.getExn(t, n_addr);
    Array2D$AdventOfCode.set(t, n_addr, orig > 0 ? orig + 1 | 0 : orig);
  });
  return t;
}

function dim(t, coord) {
  Array2D$AdventOfCode.set(t, coord, 0);
}

function iterate(t) {
  let next = increaseEnergy(t);
  while (true) {
    let flashings = getFlashingCoords(next);
    if (flashings.length === 0) {
      return next;
    }
    Belt_Array.forEach(flashings, flash_coord => {
      Array2D$AdventOfCode.set(performFlash(next, flash_coord), flash_coord, 0);
    });
    continue;
  };
}

function iterateAndReduceN(_t, _n, _acc, reducer) {
  while (true) {
    let acc = _acc;
    let n = _n;
    let t = _t;
    let next = iterate(t);
    let acc$1 = reducer(acc, t);
    if ((n - 1 | 0) < 0) {
      return acc$1;
    }
    _acc = acc$1;
    _n = n - 1 | 0;
    _t = next;
    continue;
  };
}

function countFlashN(t, n) {
  return iterateAndReduceN(t, n, 0, (acc, t) => acc + countZero(Array2D$AdventOfCode.flatten(t)) | 0);
}

function iterateN(t, n) {
  return iterateAndReduceN(t, n, t, (param, t) => t);
}

function flashesAtN(t, n) {
  return iterateAndReduceN(t, n, 0, (param, t) => countZero(Array2D$AdventOfCode.flatten(t)));
}

function toString(t) {
  let ret = [];
  for (let i = 0, i_finish = Array2D$AdventOfCode.lengthY(t); i < i_finish; ++i) {
    let row = Belt_Option.getWithDefault(Array2D$AdventOfCode.getYEquals(t, i), []);
    ret = Belt_Array.concat(ret, [Belt_Array.map(row, x => x.toString()).join("")]);
  }
  return ret.join("\n");
}

let Octopus = {
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
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), extra => Stdlib__Function.compose(prim => prim.trim(), x => Belt_Array.map(Utils$AdventOfCode.splitChars(x), Utils$AdventOfCode.intFromStringExn), extra));
}

function solvePart1_try(data) {
  console.log("orig ----");
  let d = parse(data);
  console.log(toString(d));
  console.log("orig ----");
  console.log("iterate " + String(1) + " ----");
  let e = iterateN(d, 1);
  console.log(toString(e));
  console.log("iterate " + String(1) + " ----");
  console.log("iterate " + String(100) + " ----");
  let e$1 = iterateN(d, 100);
  console.log(toString(e$1));
  console.log("iterate " + String(100) + " ----");
}

function solvePart1(data) {
  let d = parse(data);
  return countFlashN(d, 100);
}

function solvePart2(data) {
  let d = parse(data);
  let i = 0;
  let c = 0;
  while (c < 100) {
    c = flashesAtN(d, i);
    i = i + 1 | 0;
  };
  return i - 1 | 0;
}

export {
  log,
  Octopus,
  parse,
  solvePart1_try,
  solvePart1,
  solvePart2,
}
/* Utils-AdventOfCode Not a pure module */
