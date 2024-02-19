// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "rescript/lib/es6/js_int.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as AOC2018_Day5_Data$AdventOfCode from "./AOC2018_Day5_Data.mjs";
import * as AOC2018_Day5_Data_Sample$AdventOfCode from "./AOC2018_Day5_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

var charArray = AOC2018_Day5_Data$AdventOfCode.data.split("");

var charList = Belt_List.fromArray(charArray);

var testCharArray = AOC2018_Day5_Data_Sample$AdventOfCode.data.split("");

var testCharList = Belt_List.fromArray(testCharArray);

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

function isLetterAndUpper(which, c) {
  var u = which.toUpperCase();
  var l = which.toLowerCase();
  if (c === u) {
    return true;
  } else {
    return c === l;
  }
}

function notIsLetterAndUpper(which, c) {
  return !isLetterAndUpper(c, which);
}

var aToz = [
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z"
];

var aTod = [
  "a",
  "b",
  "c",
  "d"
];

function makeATozData(letters, data) {
  return Belt_Array.reduce(letters, undefined, (function (a, c) {
                var v = Belt_Array.keep(data, (function (param) {
                        return notIsLetterAndUpper(c, param);
                      }));
                return Belt_MapString.set(a, c, v);
              }));
}

function solvePart1(_d) {
  return 240;
}

function solvePart2(polymars, d) {
  return Belt_MapString.reduce(Belt_MapString.map(Belt_MapString.map(makeATozData(polymars, d), defuse_array), (function (prim) {
                    return prim.length;
                  })), Js_int.max, (function (a, _k, v) {
                if (v < a) {
                  return v;
                } else {
                  return a;
                }
              }));
}

var data = AOC2018_Day5_Data$AdventOfCode.data;

var sampleData = AOC2018_Day5_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  log ,
  charArray ,
  charList ,
  testCharArray ,
  testCharList ,
  groupByN ,
  groupBy2 ,
  groupBy3 ,
  fuse ,
  findPairIndex ,
  findPairIndex_array ,
  defuse ,
  defuse_array ,
  isLetterAndUpper ,
  notIsLetterAndUpper ,
  aToz ,
  aTod ,
  makeATozData ,
  solvePart1 ,
  solvePart2 ,
}
/* charArray Not a pure module */
