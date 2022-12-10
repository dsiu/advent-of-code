// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as TableclothSet from "tablecloth-rescript/src/TableclothSet.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Stdlib_Array$AdventOfCode from "../../stdlib/Stdlib_Array.mjs";

function log(prim) {
  console.log(prim);
}

function inRange(range, pixel) {
  var py = pixel[1];
  var px = pixel[0];
  var match = range[1];
  var by = match[1];
  var bx = match[0];
  var match$1 = range[0];
  var ay = match$1[1];
  var ax = match$1[0];
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

var neighbours = Stdlib_Array$AdventOfCode.combination2([
      -1,
      0,
      1
    ], [
      -1,
      0,
      1
    ], (function (x, y) {
        return [
                x,
                y
              ];
      }));

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                return prim.trim();
              }));
}

function solvePart1(data) {
  return 1;
}

function solvePart2(data) {
  return 2;
}

var TC;

var Pixel;

export {
  log ,
  TC ,
  Pixel ,
  inRange ,
  findContents ,
  neighbours ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* neighbours Not a pure module */
