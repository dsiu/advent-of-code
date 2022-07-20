// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_SortArrayInt = require("rescript/lib/js/belt_SortArrayInt.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");

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
                return Belt_Option.getExn(Belt_Int.fromString(x));
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

exports.log = log;
exports.median = median;
exports.distance = distance;
exports.cost = cost;
exports.costAll = costAll;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
