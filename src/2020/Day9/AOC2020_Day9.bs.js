// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Belt_Result = require("rescript/lib/js/belt_Result.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");

function codes(t) {
  return t.codes;
}

function runLength(t) {
  return t.runLength;
}

function isSumofWith(xs, x, sum) {
  return Belt_Array.keep(xs, (function (a) {
                if (a !== x) {
                  return (a + x | 0) === sum;
                } else {
                  return false;
                }
              }));
}

function isSumOf(xs, sum) {
  return Belt_Array.keep(xs, (function (a) {
                return isSumofWith(xs, a, sum).length !== 0;
              }));
}

function isCodeValid(t, i) {
  var lastSet = Belt_Array.slice(t.codes, i - t.runLength | 0, t.runLength);
  var c = Belt_Array.get(t.codes, i);
  if (c !== undefined) {
    Utils$AdventOfCode.log(c);
    Utils$AdventOfCode.log(lastSet);
    return {
            TAG: /* Ok */0,
            _0: isSumOf(lastSet, c)
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: /* InvalidIndex */1
          };
  }
}

function make(codes, runLength) {
  Utils$AdventOfCode.log(runLength);
  Utils$AdventOfCode.log(codes.length);
  if (codes.length > runLength) {
    return {
            TAG: /* Ok */0,
            _0: {
              codes: codes,
              runLength: runLength
            }
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: /* InvalidRunLength */0
          };
  }
}

var Xmax = {
  codes: codes,
  runLength: runLength,
  isSumofWith: isSumofWith,
  isSumOf: isSumOf,
  isCodeValid: isCodeValid,
  make: make
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return Belt_Option.getExn(Belt_Int.fromString(x.trim()));
              }));
}

function solvePart1(data) {
  var xmax = make(parse(data), 5);
  Utils$AdventOfCode.log(Belt_Result.getExn(xmax));
  return Belt_Result.getExn(isCodeValid(Belt_Result.getExn(xmax), 14));
}

function solvePart2(data) {
  return 2;
}

var log = Utils$AdventOfCode.log;

exports.log = log;
exports.Xmax = Xmax;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
