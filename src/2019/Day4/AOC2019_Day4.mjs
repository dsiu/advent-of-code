// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
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

var ret = {
  contents: []
};

Belt_Array.forEach(Belt_Array.range(1, 6), (function (d1) {
        Belt_Array.forEach(Belt_Array.range(d1, 9), (function (d2) {
                Belt_Array.forEach(Belt_Array.range(d2, 9), (function (d3) {
                        Belt_Array.forEach(Belt_Array.range(d3, 9), (function (d4) {
                                Belt_Array.forEach(Belt_Array.range(d4, 9), (function (d5) {
                                        Belt_Array.forEach(Belt_Array.range(d5, 9), (function (d6) {
                                                ret.contents = Belt_Array.concat(ret.contents, [[
                                                        d1,
                                                        d2,
                                                        d3,
                                                        d4,
                                                        d5,
                                                        d6
                                                      ]]);
                                              }));
                                      }));
                              }));
                      }));
              }));
      }));

var candidates = ret.contents;

function numify(param) {
  return param[0] * Math.pow(10.0, 5.0) + param[1] * Math.pow(10.0, 4.0) + param[2] * Math.pow(10.0, 3.0) + param[3] * Math.pow(10.0, 2.0) + param[4] * Math.pow(10.0, 1.0) + param[5];
}

function adjacentSame(param) {
  var d5 = param[4];
  var d4 = param[3];
  var d3 = param[2];
  var d2 = param[1];
  if (Caml_obj.equal(param[0], d2) || Caml_obj.equal(d2, d3) || Caml_obj.equal(d3, d4) || Caml_obj.equal(d4, d5)) {
    return true;
  } else {
    return Caml_obj.equal(d5, param[5]);
  }
}

function isolatedAdjacentSame(param) {
  var d6 = param[5];
  var d5 = param[4];
  var d4 = param[3];
  var d3 = param[2];
  var d2 = param[1];
  var d1 = param[0];
  if (Caml_obj.equal(d1, d2) && Caml_obj.notequal(d2, d3) || Caml_obj.notequal(d1, d2) && Caml_obj.equal(d2, d3) && Caml_obj.notequal(d3, d4) || Caml_obj.notequal(d2, d3) && Caml_obj.equal(d3, d4) && Caml_obj.notequal(d4, d5) || Caml_obj.notequal(d3, d4) && Caml_obj.equal(d4, d5) && Caml_obj.notequal(d5, d6)) {
    return true;
  } else if (Caml_obj.notequal(d4, d5)) {
    return Caml_obj.equal(d5, d6);
  } else {
    return false;
  }
}

function inRange(lowerLimit, upperLimit, digits) {
  var n = numify(digits);
  if (n >= lowerLimit) {
    return n <= upperLimit;
  } else {
    return false;
  }
}

function part1(lowerLimit, upperLimit) {
  return Belt_Array.keep(Belt_Array.keep(candidates, adjacentSame), (function (__x) {
                return inRange(lowerLimit, upperLimit, __x);
              })).length;
}

function part2(lowerLimit, upperLimit) {
  return Belt_Array.keep(Belt_Array.keep(candidates, isolatedAdjacentSame), (function (__x) {
                return inRange(lowerLimit, upperLimit, __x);
              })).length;
}

function solvePart1(data) {
  return part1(231832.0, 767346.0);
}

function solvePart2(data) {
  return part2(231832.0, 767346.0);
}

export {
  log ,
  parse ,
  candidates ,
  numify ,
  adjacentSame ,
  isolatedAdjacentSame ,
  inRange ,
  part1 ,
  part2 ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
