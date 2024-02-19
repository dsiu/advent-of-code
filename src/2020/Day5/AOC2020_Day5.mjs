// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_SortArrayInt from "rescript/lib/es6/belt_SortArrayInt.js";

function log(prim) {
  console.log(prim);
}

function make(zero, one, raw) {
  return {
          nBits: raw.length,
          zero: zero,
          one: one,
          raw: raw
        };
}

function replace(src, from, to_) {
  if (Caml_obj.equal(src, from)) {
    return to_;
  } else {
    return src;
  }
}

function get(t) {
  var __x = (function (__x) {
        return __x.join("");
      })(Belt_Array.map(t.raw.split(""), (function (c) {
              return (function (__x) {
                          return replace(__x, t.one, "1");
                        })((function (__x) {
                              return replace(__x, t.zero, "0");
                            })(c));
            })));
  return parseInt(__x, 2);
}

var BitString = {
  make: make,
  replace: replace,
  get: get
};

function make$1(code) {
  return {
          row: make("F", "B", code.slice(0, 7)),
          column: make("L", "R", (function (__x) {
                    return __x.slice(7, 10);
                  })(code))
        };
}

function getRow(t) {
  return get(t.row);
}

function getColumn(t) {
  return get(t.column);
}

function getSeatId(t) {
  return (get(t.row) << 3) + get(t.column) | 0;
}

var BoardingPass = {
  make: make$1,
  getRow: getRow,
  getColumn: getColumn,
  getSeatId: getSeatId
};

function parse(data) {
  return Belt_Array.map(data.split("\n"), (function (prim) {
                return prim.trim();
              }));
}

function maxReducer(a, x) {
  if (Caml_obj.greaterthan(x, a)) {
    return x;
  } else {
    return a;
  }
}

function findGap(a, x) {
  if ((x - a | 0) === 1) {
    return x;
  } else {
    return a;
  }
}

function solvePart1(data) {
  var passes = Belt_Array.map(parse(data), make$1);
  var seatIds = Belt_Array.map(passes, getSeatId);
  return Belt_Array.reduce(seatIds, 0, maxReducer);
}

function solvePart2(data) {
  var passes = Belt_Array.map(parse(data), make$1);
  var seatIds = Belt_Array.map(passes, getSeatId);
  var sortedSeatIds = Belt_SortArrayInt.stableSort(seatIds);
  var init = Belt_Array.getExn(sortedSeatIds, 0) - 1 | 0;
  return (function (__x) {
              return Belt_Array.reduce(__x, init, findGap);
            })(sortedSeatIds) + 1 | 0;
}

export {
  log ,
  BitString ,
  BoardingPass ,
  parse ,
  maxReducer ,
  findGap ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
