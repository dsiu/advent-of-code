// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as TableclothSet from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothSet.res.mjs";

function log(prim) {
  console.log(prim);
}

function inRange(range, pixel) {
  let py = pixel[1];
  let px = pixel[0];
  let match = range[1];
  let by = match[1];
  let bx = match[0];
  let match$1 = range[0];
  let ay = match$1[1];
  let ax = match$1[0];
  if (px <= ax && px >= bx || px >= ax && px <= bx) {
    if (py <= ay && py >= by) {
      return true;
    } else if (py >= ay) {
      return py <= by;
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function findContents(grid, distant, region, here) {
  if (inRange(region, here)) {
    return TableclothSet.includes(grid, here);
  } else {
    return distant;
  }
}

let neighbours = Stdlib__Array.combination2([
  -1,
  0,
  1
], [
  -1,
  0,
  1
], (x, y) => [
  x,
  y
]);

function parse(data) {
  return Belt_Array.map(Utils.splitNewline(data), prim => prim.trim());
}

function solvePart1(data) {
  return 1;
}

function solvePart2(data) {
  return 2;
}

let TC;

let Pixel;

export {
  log,
  TC,
  Pixel,
  inRange,
  findContents,
  neighbours,
  parse,
  solvePart1,
  solvePart2,
}
/* neighbours Not a pure module */