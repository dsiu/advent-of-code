// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function log3(prim0, prim1, prim2) {
  console.log(prim0, prim1, prim2);
}

var $$Math = {};

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

var P;

export {
  log ,
  log2 ,
  log3 ,
  P ,
  $$Math ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */