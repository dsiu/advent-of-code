// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_SetString from "rescript/lib/es6/belt_SetString.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function parsePart1(x) {
  return Utils$AdventOfCode.splitChars(Utils$AdventOfCode.join(Belt_Array.map(Utils$AdventOfCode.splitNewline(x), (function (prim) {
                        return prim.trim();
                      }))));
}

function countUnique(x) {
  return Belt_SetString.size(Belt_SetString.fromArray(x));
}

function parse(data, f) {
  return Belt_Array.map(Utils$AdventOfCode.splitDoubleNewline(data), f);
}

var filled = Belt_SetString.fromArray(Utils$AdventOfCode.splitChars("abcdefghijklmnopqrstuvwxyz"));

function parsePart2(x) {
  return Belt_SetString.size((function (__x) {
                  return Belt_Array.reduce(__x, filled, Belt_SetString.intersect);
                })(Belt_Array.map(Belt_Array.map(Belt_Array.map(Utils$AdventOfCode.splitNewline(x), (function (prim) {
                                return prim.trim();
                              })), Utils$AdventOfCode.splitChars), Belt_SetString.fromArray)));
}

function solvePart1(data) {
  return Utils$AdventOfCode.sumIntArray(Belt_Array.map(parse(data, parsePart1), countUnique));
}

function solvePart2(data) {
  return Utils$AdventOfCode.sumIntArray(parse(data, parsePart2));
}

export {
  log ,
  parsePart1 ,
  countUnique ,
  parse ,
  filled ,
  parsePart2 ,
  solvePart1 ,
  solvePart2 ,
}
/* filled Not a pure module */
