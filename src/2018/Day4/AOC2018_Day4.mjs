// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Range from "rescript/lib/es6/belt_Range.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Belt_MutableMapInt from "rescript/lib/es6/belt_MutableMapInt.js";
import * as Belt_MutableSetInt from "rescript/lib/es6/belt_MutableSetInt.js";
import * as Belt_SortArrayString from "rescript/lib/es6/belt_SortArrayString.js";
import * as Belt_MutableMapString from "rescript/lib/es6/belt_MutableMapString.js";
import * as AOC2018_Day4_Data$AdventOfCode from "./AOC2018_Day4_Data.mjs";
import * as AOC2018_Day4_Data_Sample$AdventOfCode from "./AOC2018_Day4_Data_Sample.mjs";

function insertHourRec(hr, from_min, to_min) {
  Belt_Range.forEach(from_min, to_min, (function (i) {
          Belt_MutableSetInt.add(hr, i);
        }));
  return Caml_option.some(hr);
}

function insertDayRec(dr, date, from_min, to_min) {
  Belt_MutableMapString.update(dr, date, (function (hr) {
          if (hr !== undefined) {
            return insertHourRec(Caml_option.valFromOption(hr), from_min, to_min);
          } else {
            return insertHourRec(Belt_MutableSetInt.make(), from_min, to_min);
          }
        }));
  return Caml_option.some(dr);
}

function insertGuardRec(gAtt, gid, date, from_min, to_min) {
  Belt_MutableMapInt.update(gAtt, gid, (function (dr) {
          if (dr !== undefined) {
            return insertDayRec(Caml_option.valFromOption(dr), date, from_min, to_min);
          } else {
            return insertDayRec(Belt_MutableMapString.make(), date, from_min, to_min);
          }
        }));
}

function minsSleptTotal(dr) {
  return Belt_MutableMapString.reduce(dr, 0, (function (a, _k, hr) {
                return a + Belt_MutableSetInt.size(hr) | 0;
              }));
}

function perGuardMinsSlept(__x) {
  return Belt_MutableMapInt.map(__x, minsSleptTotal);
}

function findLaziestGuard(gAtt) {
  return Belt_MutableMapInt.reduce(Belt_MutableMapInt.map(gAtt, minsSleptTotal), [
              -1,
              -1
            ], (function (a, k, v) {
                if (v > a[1]) {
                  return [
                          k,
                          v
                        ];
                } else {
                  return a;
                }
              }));
}

function tallySleptPerMin(dr) {
  return Belt_MutableMapString.reduce(dr, Belt_MutableMapInt.make(), (function (a, _k, hr) {
                Belt_MutableSetInt.forEach(hr, (function (m) {
                        Belt_MutableMapInt.update(a, m, (function (prev) {
                                if (prev !== undefined) {
                                  return prev + 1 | 0;
                                } else {
                                  return 1;
                                }
                              }));
                      }));
                return a;
              }));
}

function perGuardTallySleptPerMin(__x) {
  return Belt_MutableMapInt.map(__x, tallySleptPerMin);
}

function perGuardMostSleptMin(gAtt) {
  return Belt_MutableMapInt.map(Belt_MutableMapInt.map(gAtt, tallySleptPerMin), (function (t) {
                return Belt_MutableMapInt.reduce(t, [
                            -1,
                            -1
                          ], (function (a, k, v) {
                              if (v > a[1]) {
                                return [
                                        k,
                                        v
                                      ];
                              } else {
                                return a;
                              }
                            }));
              }));
}

function busiestMin(gAtt) {
  return Belt_MutableMapInt.reduce(perGuardMostSleptMin(gAtt), [
              -1,
              [
                -1,
                -1
              ]
            ], (function (a, k, v) {
                if (v[1] > a[1][1]) {
                  return [
                          k,
                          v
                        ];
                } else {
                  return a;
                }
              }));
}

function toString(gAtt) {
  Belt_MutableMapInt.forEach(gAtt, (function (gid, drs) {
          console.log("gid:" + String(gid));
          Belt_MutableMapString.forEach(drs, (function (date, hr) {
                  console.log("  date:" + date);
                  console.log("    Set Size:" + String(Belt_MutableSetInt.size(hr)));
                  console.log(Belt_MutableSetInt.toArray(hr));
                }));
        }));
}

var Attendance = {
  insertHourRec: insertHourRec,
  insertDayRec: insertDayRec,
  insertGuardRec: insertGuardRec,
  minsSleptPerHourRec: Belt_MutableSetInt.size,
  minsSleptTotal: minsSleptTotal,
  perGuardMinsSlept: perGuardMinsSlept,
  findLaziestGuard: findLaziestGuard,
  tallySleptPerMin: tallySleptPerMin,
  perGuardTallySleptPerMin: perGuardTallySleptPerMin,
  perGuardMostSleptMin: perGuardMostSleptMin,
  busiestMin: busiestMin,
  toString: toString
};

var guardBeginsRe = /\[(.*)\s+(\d\d):(\d\d)\]\s+Guard\s+#(\d+)\s+begins shift/i;

var guardAsleepRe = /\[(.*)\s+(\d\d):(\d\d)\]\s+falls asleep/i;

var guardWakeRe = /\[(.*)\s+(\d\d):(\d\d)\]\s+wakes up/i;

function parseRegexResult(r) {
  return Belt_Array.map(r, (function (x) {
                return Belt_Option.getExn((x == null) ? undefined : Caml_option.some(x));
              }));
}

function unboxBeginLine(l) {
  if (l.length !== 5) {
    throw {
          RE_EXN_ID: "Match_failure",
          _1: [
            "AOC2018_Day4.res",
            149,
            8
          ],
          Error: new Error()
        };
  }
  var raw = l[0];
  var date = l[1];
  var h = l[2];
  var m = l[3];
  var gid = l[4];
  return {
          raw: raw,
          date: date,
          h: Caml_format.int_of_string(h),
          m: Caml_format.int_of_string(m),
          gid: Caml_format.int_of_string(gid)
        };
}

function unboxAsleepLine(l) {
  if (l.length !== 4) {
    throw {
          RE_EXN_ID: "Match_failure",
          _1: [
            "AOC2018_Day4.res",
            154,
            8
          ],
          Error: new Error()
        };
  }
  var raw = l[0];
  var date = l[1];
  var h = l[2];
  var m = l[3];
  return {
          raw: raw,
          date: date,
          h: Caml_format.int_of_string(h),
          m: Caml_format.int_of_string(m),
          gid: -1
        };
}

function unboxAwakeLine(l) {
  if (l.length !== 4) {
    throw {
          RE_EXN_ID: "Match_failure",
          _1: [
            "AOC2018_Day4.res",
            159,
            8
          ],
          Error: new Error()
        };
  }
  var raw = l[0];
  var date = l[1];
  var h = l[2];
  var m = l[3];
  return {
          raw: raw,
          date: date,
          h: Caml_format.int_of_string(h),
          m: Caml_format.int_of_string(m),
          gid: -1
        };
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
    return {
            TAG: "Begin",
            _0: unboxBeginLine(parseRegexResult(match))
          };
  }
  if (match$1 !== null) {
    if (match$2 !== null) {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    return {
            TAG: "Asleep",
            _0: unboxAsleepLine(parseRegexResult(match$1))
          };
  }
  if (match$2 !== null) {
    return {
            TAG: "Awake",
            _0: unboxAwakeLine(parseRegexResult(match$2))
          };
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function processLineReducer(a, x) {
  var state = a.state;
  var d = parseLine(x);
  switch (d.TAG) {
    case "Begin" :
        return {
                state: "AtBegin",
                gid: d._0.gid,
                sleptSince: -1,
                gAtt: a.gAtt
              };
    case "Asleep" :
        if (!(state === "AtAwake" || state === "AtBegin")) {
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "AOC2018_Day4.res",
                  200,
                  8
                ],
                Error: new Error()
              };
        }
        return {
                state: "AtAsleep",
                gid: a.gid,
                sleptSince: d._0.m,
                gAtt: a.gAtt
              };
    case "Awake" :
        var d$1 = d._0;
        if (state !== "AtAsleep") {
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "AOC2018_Day4.res",
                  206,
                  8
                ],
                Error: new Error()
              };
        }
        insertGuardRec(a.gAtt, a.gid, d$1.date, a.sleptSince, d$1.m - 1 | 0);
        return {
                state: "AtAwake",
                gid: a.gid,
                sleptSince: -1,
                gAtt: a.gAtt
              };
    
  }
}

var Parser = {
  guardBeginsRe: guardBeginsRe,
  guardAsleepRe: guardAsleepRe,
  guardWakeRe: guardWakeRe,
  parseRegexResult: parseRegexResult,
  unboxBeginLine: unboxBeginLine,
  unboxAsleepLine: unboxAsleepLine,
  unboxAwakeLine: unboxAwakeLine,
  parseLine: parseLine,
  processLineReducer: processLineReducer
};

function solvePart1(data) {
  var sortLines = Belt_SortArrayString.stableSort(data.split("\n"));
  var initState_gAtt = Belt_MutableMapInt.make();
  var initState = {
    state: "AtInit",
    gid: 0,
    sleptSince: 0,
    gAtt: initState_gAtt
  };
  var match = Belt_Array.reduce(sortLines, initState, processLineReducer);
  var gAtt = match.gAtt;
  var laziest = findLaziestGuard(gAtt);
  var laziestMins = perGuardMostSleptMin(gAtt);
  var laziestGid = laziest[0];
  var match$1 = Belt_Option.getExn(Belt_MutableMapInt.get(laziestMins, laziestGid));
  return Math.imul(laziestGid, match$1[0]);
}

function solvePart2(data) {
  var sortLines = Belt_SortArrayString.stableSort(data.split("\n"));
  var initState_gAtt = Belt_MutableMapInt.make();
  var initState = {
    state: "AtInit",
    gid: 0,
    sleptSince: 0,
    gAtt: initState_gAtt
  };
  var match = Belt_Array.reduce(sortLines, initState, processLineReducer);
  var match$1 = busiestMin(match.gAtt);
  return Math.imul(match$1[0], match$1[1][0]);
}

var data = AOC2018_Day4_Data$AdventOfCode.data;

var sampleData = AOC2018_Day4_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  Attendance ,
  Parser ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
