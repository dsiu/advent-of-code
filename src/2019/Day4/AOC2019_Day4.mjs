// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), prim => prim.trim());
}

let ret = {
  contents: []
};

Belt_Array.forEach(Belt_Array.range(1, 6), d1 => Belt_Array.forEach(Belt_Array.range(d1, 9), d2 => Belt_Array.forEach(Belt_Array.range(d2, 9), d3 => Belt_Array.forEach(Belt_Array.range(d3, 9), d4 => Belt_Array.forEach(Belt_Array.range(d4, 9), d5 => Belt_Array.forEach(Belt_Array.range(d5, 9), d6 => {
  ret.contents = Belt_Array.concat(ret.contents, [[
      d1,
      d2,
      d3,
      d4,
      d5,
      d6
    ]]);
}))))));

let candidates = ret.contents;

function numify(param) {
  return param[0] * Math.pow(10.0, 5.0) + param[1] * Math.pow(10.0, 4.0) + param[2] * Math.pow(10.0, 3.0) + param[3] * Math.pow(10.0, 2.0) + param[4] * Math.pow(10.0, 1.0) + param[5];
}

function adjacentSame(param) {
  let d5 = param[4];
  let d4 = param[3];
  let d3 = param[2];
  let d2 = param[1];
  if (Primitive_object.equal(param[0], d2) || Primitive_object.equal(d2, d3) || Primitive_object.equal(d3, d4) || Primitive_object.equal(d4, d5)) {
    return true;
  } else {
    return Primitive_object.equal(d5, param[5]);
  }
}

function isolatedAdjacentSame(param) {
  let d6 = param[5];
  let d5 = param[4];
  let d4 = param[3];
  let d3 = param[2];
  let d2 = param[1];
  let d1 = param[0];
  if (Primitive_object.equal(d1, d2) && Primitive_object.notequal(d2, d3) || Primitive_object.notequal(d1, d2) && Primitive_object.equal(d2, d3) && Primitive_object.notequal(d3, d4) || Primitive_object.notequal(d2, d3) && Primitive_object.equal(d3, d4) && Primitive_object.notequal(d4, d5) || Primitive_object.notequal(d3, d4) && Primitive_object.equal(d4, d5) && Primitive_object.notequal(d5, d6)) {
    return true;
  } else if (Primitive_object.notequal(d4, d5)) {
    return Primitive_object.equal(d5, d6);
  } else {
    return false;
  }
}

function inRange(lowerLimit, upperLimit, digits) {
  let n = numify(digits);
  if (n >= lowerLimit) {
    return n <= upperLimit;
  } else {
    return false;
  }
}

function part1(lowerLimit, upperLimit) {
  return Belt_Array.keep(Belt_Array.keep(candidates, adjacentSame), __x => inRange(lowerLimit, upperLimit, __x)).length;
}

function part2(lowerLimit, upperLimit) {
  return Belt_Array.keep(Belt_Array.keep(candidates, isolatedAdjacentSame), __x => inRange(lowerLimit, upperLimit, __x)).length;
}

function solvePart1(data) {
  return part1(231832.0, 767346.0);
}

function solvePart2(data) {
  return part2(231832.0, 767346.0);
}

export {
  log,
  parse,
  candidates,
  numify,
  adjacentSame,
  isolatedAdjacentSame,
  inRange,
  part1,
  part2,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
