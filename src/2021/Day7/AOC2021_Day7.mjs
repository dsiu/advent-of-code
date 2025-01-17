// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_SortArrayInt from "rescript/lib/es6/belt_SortArrayInt.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function median(xs) {
  var sorted = Belt_SortArrayInt.stableSort(xs);
  var half = (xs.length >> 1);
  if (xs.length % 2 === 0) {
    return Belt_Array.getExn(sorted, half);
  } else {
    return (Belt_Array.getExn(sorted, half - 1 | 0) + Belt_Array.getExn(sorted, half) | 0) / 2 | 0;
  }
}

function distance(xs, m) {
  return Belt_Array.reduce(xs, 0, (function (a, x) {
                return a + Math.abs(x - m | 0) | 0;
              }));
}

function cost(a, b) {
  return Math.imul(Math.abs(b - a | 0), Math.abs(b - a | 0) + 1 | 0) / 2 | 0;
}

function costAll(xs, p) {
  return Belt_Array.reduce(xs, 0, (function (a, x) {
                return a + cost(x, p) | 0;
              }));
}

function parse(data) {
  return Belt_Array.map(data.trim().split(","), (function (x) {
                return Utils$AdventOfCode.intFromStringExn(x);
              }));
}

function solvePart1(data) {
  var xs = parse(data);
  var m = median(xs);
  return distance(xs, m);
}

function solvePart2(data) {
  var xs = parse(data);
  var min = Utils$AdventOfCode.minIntInArray(xs);
  var max = Utils$AdventOfCode.maxIntInArray(xs);
  return Utils$AdventOfCode.minIntInArray(Belt_Array.map(Belt_Array.rangeBy(min, max, 1), (function (__x) {
                    return costAll(xs, __x);
                  })));
}

export {
  log ,
  median ,
  distance ,
  cost ,
  costAll ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
