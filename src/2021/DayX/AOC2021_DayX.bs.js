// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");

function log(prim) {
  console.log(prim);
  
}

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

exports.log = log;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
