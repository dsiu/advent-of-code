// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("@rescript/std/lib/js/curry.js");
var Belt_Array = require("@rescript/std/lib/js/belt_Array.js");
var Caml_array = require("@rescript/std/lib/js/caml_array.js");
var Belt_MapString = require("@rescript/std/lib/js/belt_MapString.js");
var Day2_Data$AdventOfCode = require("./Day2_Data.bs.js");

function string_to_charStr(param) {
  return param.split("");
}

function reducer(r, x) {
  var count = Belt_MapString.get(r, x);
  if (count !== undefined) {
    return Belt_MapString.set(r, x, count + 1 | 0);
  } else {
    return Belt_MapString.set(r, x, 1);
  }
}

function char_freq(cs) {
  return Belt_Array.reduce(cs, undefined, reducer);
}

function countMatchFreq(freq, m) {
  return Belt_MapString.reduce(m, 0, (function (acc, key, v) {
                if (v === freq) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }));
}

function n_char_matched_freq(freq, s) {
  var cs = s.split("");
  return countMatchFreq(freq, Belt_Array.reduce(cs, undefined, reducer));
}

function twoTimesCounter(param) {
  return n_char_matched_freq(2, param);
}

function threeTimesCounter(param) {
  return n_char_matched_freq(3, param);
}

function nonZero(x) {
  if (x !== 0) {
    return 1;
  } else {
    return 0;
  }
}

function runDay2Part1(lines) {
  var result = Belt_Array.reduce(lines, {
        twoTimes: 0,
        threeTimes: 0
      }, (function (acc, l) {
          return {
                  twoTimes: acc.twoTimes + nonZero(n_char_matched_freq(2, l)) | 0,
                  threeTimes: acc.threeTimes + nonZero(n_char_matched_freq(3, l)) | 0
                };
        }));
  return Math.imul(result.twoTimes, result.threeTimes);
}

function diffOfTwoCharStr(s1, s2) {
  var s1CharStr = s1.split("");
  var s2CharStr = s2.split("");
  return Belt_Array.mapWithIndex(s1CharStr, (function (i, x) {
                var y = Belt_Array.get(s2CharStr, i);
                if (y !== undefined) {
                  if (x === y) {
                    return {
                            TAG: /* Match */0,
                            _0: x
                          };
                  } else {
                    return {
                            TAG: /* NotMatch */1,
                            _0: x,
                            _1: y
                          };
                  }
                } else {
                  return {
                          TAG: /* NotMatch */1,
                          _0: x,
                          _1: x
                        };
                }
              }));
}

function countTrue(xs) {
  return Belt_Array.keep(xs, (function (x) {
                if (x.TAG === /* Match */0) {
                  return true;
                } else {
                  return false;
                }
              })).length;
}

function countFalse(xs) {
  return Belt_Array.keep(xs, (function (x) {
                if (x.TAG === /* Match */0) {
                  return false;
                } else {
                  return true;
                }
              })).length;
}

function isDiffBy(n, xs) {
  return countFalse(xs) === n;
}

function isDiffBy1(param) {
  return countFalse(param) === 1;
}

function isDiffBy5(param) {
  return countFalse(param) === 5;
}

function findMatch(lines, predicate, x) {
  return Belt_Array.reduce(lines, {
              src: x,
              matched: []
            }, (function (a, y) {
                var match = Curry._1(predicate, diffOfTwoCharStr(a.src, y));
                return {
                        src: a.src,
                        matched: match ? Belt_Array.concat(a.matched, [y]) : a.matched
                      };
              }));
}

function findAllMatch(predicate, lines) {
  return Belt_Array.reduce(lines, [], (function (a, x) {
                var res = findMatch(lines, predicate, x);
                var match = res.matched.length;
                if (match !== 0) {
                  return Belt_Array.concat(a, [res]);
                } else {
                  return a;
                }
              }));
}

function runDay2Part2(lines) {
  return Belt_Array.map(findAllMatch(isDiffBy1, lines), (function (x) {
                return Belt_Array.reduce(diffOfTwoCharStr(x.src, Caml_array.get(x.matched, 0)), "", (function (a, x) {
                              if (x.TAG === /* Match */0) {
                                return a + x._0;
                              } else {
                                return a;
                              }
                            }));
              }));
}

var data = Day2_Data$AdventOfCode.data;

var test_string = "aabbbccccccddddd";

exports.data = data;
exports.string_to_charStr = string_to_charStr;
exports.reducer = reducer;
exports.char_freq = char_freq;
exports.countMatchFreq = countMatchFreq;
exports.n_char_matched_freq = n_char_matched_freq;
exports.twoTimesCounter = twoTimesCounter;
exports.threeTimesCounter = threeTimesCounter;
exports.nonZero = nonZero;
exports.runDay2Part1 = runDay2Part1;
exports.test_string = test_string;
exports.diffOfTwoCharStr = diffOfTwoCharStr;
exports.countTrue = countTrue;
exports.countFalse = countFalse;
exports.isDiffBy = isDiffBy;
exports.isDiffBy1 = isDiffBy1;
exports.isDiffBy5 = isDiffBy5;
exports.findMatch = findMatch;
exports.findAllMatch = findAllMatch;
exports.runDay2Part2 = runDay2Part2;
/* No side effect */
