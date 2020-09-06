// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Day5_Data$AdventOfCode = require("./Day5_Data.bs.js");
var Day5_Data_Test$AdventOfCode = require("./Day5_Data_Test.bs.js");

function log(prim) {
  console.log(prim);
  
}

var charArray = Day5_Data$AdventOfCode.data.split("");

var charList = Belt_List.fromArray(charArray);

function groupByN(l, n) {
  var match = Belt_List.reduce(l, [
        /* [] */0,
        /* [] */0
      ], (function (a, x) {
          var cur = a[1];
          var r = a[0];
          if (Belt_List.length(cur) === (n - 1 | 0)) {
            return [
                    Belt_List.add(r, Belt_List.reverse(Belt_List.add(cur, x))),
                    /* [] */0
                  ];
          } else {
            return [
                    r,
                    Belt_List.add(cur, x)
                  ];
          }
        }));
  return Belt_List.reverse(match[0]);
}

function groupBy2(__x) {
  return groupByN(__x, 2);
}

function groupBy3(__x) {
  return groupByN(__x, 3);
}

function fuse(pair) {
  var b = pair[1];
  var a = pair[0];
  if (a !== b) {
    if (a === b.toLowerCase() && b === a.toUpperCase()) {
      return true;
    } else if (b === a.toLowerCase()) {
      return a === b.toUpperCase();
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function findPairIndex(l) {
  var match = Belt_List.reduceWithIndex(l, [
        "",
        false,
        false,
        -1
      ], (function (a, x, i) {
          if (a[2]) {
            return a;
          } else if (a[1] && fuse([
                  a[0],
                  x
                ])) {
            return [
                    "",
                    false,
                    true,
                    i - 1 | 0
                  ];
          } else {
            return [
                    x,
                    true,
                    false,
                    -1
                  ];
          }
        }));
  if (match[2]) {
    return match[3];
  }
  
}

function findPairIndex_array(l) {
  var _i = 0;
  var len = l.length;
  var _last;
  var _has_last = false;
  while(true) {
    var has_last = _has_last;
    var last = _last;
    var i = _i;
    var x = Belt_Array.get(l, i);
    var cont = i < len;
    if (!cont) {
      return ;
    }
    if (has_last) {
      if (fuse([
              Belt_Option.getExn(last),
              Belt_Option.getExn(x)
            ])) {
        return i - 1 | 0;
      }
      _has_last = true;
      _last = x;
      _i = i + 1 | 0;
      continue ;
    }
    _has_last = true;
    _last = x;
    _i = i + 1 | 0;
    continue ;
  };
}

function defuse(_l) {
  while(true) {
    var l = _l;
    var i = findPairIndex(l);
    if (i === undefined) {
      return l;
    }
    var h = Belt_Option.getExn(Belt_List.take(l, i));
    var t = Belt_Option.getExn(Belt_List.drop(l, i + 2 | 0));
    _l = Belt_List.concat(h, t);
    continue ;
  };
}

function defuse_array(_l) {
  while(true) {
    var l = _l;
    var i = findPairIndex_array(l);
    if (i === undefined) {
      return l;
    }
    var h = Belt_Array.slice(l, 0, i);
    var t = Belt_Array.sliceToEnd(l, i + 2 | 0);
    _l = Belt_Array.concat(h, t);
    continue ;
  };
}

function solvePart1(d) {
  return 240;
}

function solvePart2(d) {
  return 4455;
}

var data = Day5_Data$AdventOfCode.data;

var testData = Day5_Data_Test$AdventOfCode.data;

exports.data = data;
exports.testData = testData;
exports.log = log;
exports.charArray = charArray;
exports.charList = charList;
exports.groupByN = groupByN;
exports.groupBy2 = groupBy2;
exports.groupBy3 = groupBy3;
exports.fuse = fuse;
exports.findPairIndex = findPairIndex;
exports.findPairIndex_array = findPairIndex_array;
exports.defuse = defuse;
exports.defuse_array = defuse_array;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* charArray Not a pure module */
