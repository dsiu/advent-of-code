// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

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

export {
  log ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
