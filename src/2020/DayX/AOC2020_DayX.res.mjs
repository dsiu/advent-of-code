// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";

function log(prim) {
  console.log(prim);
}

function parse(data) {
  return Belt_Array.map(Utils.splitNewline(data), prim => prim.trim());
}

function solvePart1(data) {
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */