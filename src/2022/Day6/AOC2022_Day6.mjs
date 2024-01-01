// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var parse = Utils$AdventOfCode.splitChars;

function allDifferent(cs) {
  return Stdlib__Array.equal(Stdlib__Array.uniq(cs), cs, (function (prim0, prim1) {
                return prim0 === prim1;
              }));
}

function hasSame(x) {
  return !allDifferent(x);
}

function interestingPosition(n, text) {
  var len = Stdlib__Array.length(text);
  var candidates = Stdlib__Array.zip(Stdlib__Array.makeBy(len, Stdlib__Function.identity), Stdlib__Array.map(Stdlib__Array.tails(text), (function (__x) {
              return Stdlib__Array.take(__x, n);
            })));
  var packetPos = Stdlib__Array.headUnsafe(Stdlib__Array.dropWhile(candidates, (function (param) {
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
/* Stdlib__Array Not a pure module */
