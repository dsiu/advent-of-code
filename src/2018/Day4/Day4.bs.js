// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_SortArrayString = require("bs-platform/lib/js/belt_SortArrayString.js");
var Day4_Data$AdventOfCode = require("./Day4_Data.bs.js");

var claimRe = /#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i;

var guardBeginsRe = /\[(.*)\]\s+Guard\s+#(\d+)\s+begins shift/i;

var guardAsleepRe = /\[(.*)\]\s+falls asleep/i;

var guardWakeRe = /\[(.*)\]\s+wakes up/i;

function parseRegexResult(r) {
  return Belt_Array.map(r, (function (x) {
                return Belt_Option.getExn((x == null) ? undefined : Caml_option.some(x));
              }));
}

function parseLine(l) {
  var trimmed = l.trim();
  var match = guardBeginsRe.exec(trimmed);
  var match$1 = guardAsleepRe.exec(trimmed);
  var match$2 = guardWakeRe.exec(trimmed);
  if (match !== null) {
    if (match$1 !== null) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    if (match$2 !== null) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    return parseRegexResult(match);
  }
  if (match$1 !== null) {
    if (match$2 !== null) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    return parseRegexResult(match$1);
  }
  if (match$2 !== null) {
    return parseRegexResult(match$2);
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var sortLines = Belt_SortArrayString.stableSort(Day4_Data$AdventOfCode.data.split("\n"));

console.log(Belt_Array.map(sortLines, parseLine));

var data = Day4_Data$AdventOfCode.data;

exports.data = data;
exports.claimRe = claimRe;
exports.guardBeginsRe = guardBeginsRe;
exports.guardAsleepRe = guardAsleepRe;
exports.guardWakeRe = guardWakeRe;
exports.parseRegexResult = parseRegexResult;
exports.parseLine = parseLine;
exports.sortLines = sortLines;
/* sortLines Not a pure module */
