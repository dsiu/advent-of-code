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

function getCode(t, i) {
  return Belt_Array.get(t.codes, i);
}

function getCodeExn(t, i) {
  return Belt_Array.getExn(t.codes, i);
}

function codeSize(t) {
  return t.codes.length;
}

function runLength(t) {
  return t.runLength;
}

function preambles(t) {
  return Belt_Array.slice(t.codes, 0, t.runLength);
}

function make(codes, runLength) {
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

function findSumOf(xs, sum) {
  var sumArray = isSumOf(xs, sum);
  if (sumArray.length === 0) {
    return ;
  } else {
    return sumArray;
  }
}

function isCodeValid(t, i) {
  var lastSet = Belt_Array.slice(t.codes, i - t.runLength | 0, t.runLength);
  var c = Belt_Array.get(t.codes, i);
  if (c !== undefined) {
    return {
            TAG: /* Ok */0,
            _0: findSumOf(lastSet, c)
          };
  } else {
    return {
            TAG: /* Error */1,
            _0: /* InvalidIndex */1
          };
  }
}

function findInvalidCode(t) {
  var _i = t.runLength;
  while(true) {
    var i = _i;
    if (i >= t.codes.length) {
      return ;
    }
    var sumOk = isCodeValid(t, i);
    if (sumOk.TAG !== /* Ok */0) {
      return ;
    }
    if (sumOk._0 === undefined) {
      return Belt_Array.getExn(t.codes, i);
    }
    _i = i + 1 | 0;
    continue ;
  };
}

var Xmax = {
  codes: codes,
  getCode: getCode,
  getCodeExn: getCodeExn,
  codeSize: codeSize,
  runLength: runLength,
  preambles: preambles,
  make: make,
  isSumofWith: isSumofWith,
  isSumOf: isSumOf,
  findSumOf: findSumOf,
  isCodeValid: isCodeValid,
  findInvalidCode: findInvalidCode
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return Belt_Option.getExn(Belt_Int.fromString(x.trim()));
              }));
}

function solvePart1(data, preambleSize) {
  var xmax = Belt_Result.getExn(make(parse(data), preambleSize));
  return Belt_Option.getExn(findInvalidCode(xmax));
}

function solvePart2(data, preambleSize) {
  var xmax = Belt_Result.getExn(make(parse(data), preambleSize));
  var badCode = Belt_Option.getExn(findInvalidCode(xmax));
  Utils$AdventOfCode.log(badCode);
  var preambles$1 = preambles(xmax);
  Utils$AdventOfCode.log(preambles$1);
  return 2;
}

var log = Utils$AdventOfCode.log;

exports.log = log;
exports.Xmax = Xmax;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
