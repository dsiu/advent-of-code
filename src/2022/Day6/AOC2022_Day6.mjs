// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib_Array from "@dsiu/rescript-stdlib-fp/src/Stdlib_Array.mjs";
import * as Stdlib_Function from "@dsiu/rescript-stdlib-fp/src/Stdlib_Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var parse = Utils$AdventOfCode.splitChars;

function allDifferent(cs) {
  return Stdlib_Array.equal(Stdlib_Array.uniq(cs), cs, (function (prim0, prim1) {
                return prim0 === prim1;
              }));
}

function hasSame(x) {
  return !allDifferent(x);
}

function interestingPosition(n, text) {
  var len = text.length;
  var candidates = Stdlib_Array.zip(Stdlib_Array.makeBy(len, Stdlib_Function.identity), Stdlib_Array.tails(text).map(function (__x) {
            return Stdlib_Array.take(__x, n);
          }));
  var packetPos = Stdlib_Array.head(Stdlib_Array.dropWhile(candidates, (function (param) {
              return Utils$AdventOfCode.compose((function (prim) {
                            return prim[1];
                          }), hasSame, param);
            })));
  return n + packetPos[0] | 0;
}

function solvePart1(data) {
  return interestingPosition(4, Utils$AdventOfCode.splitChars(data));
}

function solvePart2(data) {
  return interestingPosition(14, Utils$AdventOfCode.splitChars(data));
}

var A;

export {
  log ,
  A ,
  parse ,
  allDifferent ,
  hasSame ,
  interestingPosition ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
