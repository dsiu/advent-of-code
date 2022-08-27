// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Js_string from "rescript/lib/es6/js_string.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as AOC2018_Day2_Data$AdventOfCode from "./AOC2018_Day2_Data.mjs";

function string_to_charStr(param) {
  return Js_string.split("", param);
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
  return Belt_MapString.reduce(m, 0, (function (acc, _key, v) {
                if (v === freq) {
                  return acc + 1 | 0;
                } else {
                  return acc;
                }
              }));
}

function n_char_matched_freq(freq, s) {
  var cs = Js_string.split("", s);
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
  var s1CharStr = Js_string.split("", s1);
  var s2CharStr = Js_string.split("", s2);
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
  return Belt_Array.keepMap(lines, (function (x) {
                var res = findMatch(lines, predicate, x);
                var match = res.matched.length;
                if (match !== 0) {
                  return res;
                }
                
              }));
}

function runDay2Part2(lines) {
  return Belt_Array.map(findAllMatch(isDiffBy1, lines), (function (x) {
                return Belt_Array.keepMap(diffOfTwoCharStr(x.src, Caml_array.get(x.matched, 0)), (function (x) {
                                if (x.TAG === /* Match */0) {
                                  return x._0;
                                }
                                
                              })).join("");
              }));
}

var data = AOC2018_Day2_Data$AdventOfCode.data;

var test_string = "aabbbccccccddddd";

export {
  data ,
  string_to_charStr ,
  reducer ,
  char_freq ,
  countMatchFreq ,
  n_char_matched_freq ,
  twoTimesCounter ,
  threeTimesCounter ,
  nonZero ,
  runDay2Part1 ,
  test_string ,
  diffOfTwoCharStr ,
  countTrue ,
  countFalse ,
  isDiffBy ,
  isDiffBy1 ,
  isDiffBy5 ,
  findMatch ,
  findAllMatch ,
  runDay2Part2 ,
}
/* No side effect */
