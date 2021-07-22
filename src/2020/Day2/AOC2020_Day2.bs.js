// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");

function log(prim) {
  console.log(prim);
  
}

var formatRe = /(\d+)-(\d+)\s+([A-Za-z]+):\s+(.*)/i;

function make(s) {
  var x = formatRe.exec(s);
  var args = x !== null ? Belt_Array.map(x, (function (x) {
            return Belt_Option.getExn((x == null) ? undefined : Caml_option.some(x));
          })) : [];
  return {
          min: Belt_Option.getExn(Belt_Int.fromString(Belt_Option.getExn(Belt_Array.get(args, 1)))),
          max: Belt_Option.getExn(Belt_Int.fromString(Belt_Option.getExn(Belt_Array.get(args, 2)))),
          char: Belt_Option.getExn(Belt_Array.get(args, 3)),
          input: Belt_Option.getExn(Belt_Array.get(args, 4))
        };
}

function isValidPart1(t) {
  var count = t.input.split(t.char).length - 1 | 0;
  if (count >= t.min) {
    return count <= t.max;
  } else {
    return false;
  }
}

function isValidPart2(t) {
  var matchFirst = t.input.charAt(t.min - 1 | 0) === t.char;
  var matchSecond = t.input.charAt(t.max - 1 | 0) === t.char;
  return matchFirst !== matchSecond;
}

var Password = {
  formatRe: formatRe,
  make: make,
  isValidPart1: isValidPart1,
  isValidPart2: isValidPart2
};

function solvePart1(data) {
  var pws = Belt_Array.map(data.split("\n"), (function (s) {
          return make(s.trim());
        }));
  return Belt_Array.keep(pws, isValidPart1).length;
}

function solvePart2(data) {
  var pws = Belt_Array.map(data.split("\n"), (function (s) {
          return make(s.trim());
        }));
  return Belt_Array.keep(pws, isValidPart2).length;
}

var data1 = "1-3 a: abcde\n            1-3 b: cdefg\n            2-9 c: ccccccccc";

exports.log = log;
exports.data1 = data1;
exports.Password = Password;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
