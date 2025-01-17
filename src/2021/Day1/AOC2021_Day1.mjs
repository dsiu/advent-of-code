// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function findDiff(xs) {
  var match = Belt_Array.reduceU(Belt_Array.sliceToEnd(xs, 1), [
        Belt_Option.getExn(Belt_Array.get(xs, 0)),
        []
      ], (function (param, x) {
          return [
                  x,
                  Belt_Array.concat(param[1], [x - param[0] | 0])
                ];
        }));
  return Belt_Array.keepU(match[1], (function (x) {
                return x > 0;
              }));
}

function findDiff2(xs) {
  var shifted = Belt_Array.sliceToEnd(xs, 1);
  return Belt_Array.keep(Belt_Array.zip(shifted, xs), (function (param) {
                return Caml_obj.greaterthan(param[0], param[1]);
              }));
}

function sum2Array(a1, a2) {
  return Belt_Array.reduceReverse2(Belt_Array.reverse(a1), Belt_Array.reverse(a2), [], (function (acc, x, y) {
                return Belt_Array.concat(acc, [x + y | 0]);
              }));
}

function roll3sum(xs) {
  var size = xs.length - 2 | 0;
  var a1 = Belt_Array.slice(xs, 0, size);
  var a2 = Belt_Array.slice(xs, 1, size);
  var a3 = Belt_Array.slice(xs, 2, size);
  return sum2Array(sum2Array(a1, a2), a3);
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return Utils$AdventOfCode.intFromStringExn(x);
              }));
}

function solvePart1(data) {
  return findDiff2(parse(data)).length;
}

function solvePart2(data) {
  return findDiff2(roll3sum(parse(data))).length;
}

export {
  log ,
  findDiff ,
  findDiff2 ,
  sum2Array ,
  roll3sum ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
