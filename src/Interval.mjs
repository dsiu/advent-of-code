// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Stdlib__Math from "@dsiu/rescript-stdlib-fp/src/Stdlib__Math.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function toString(param) {
  return "Interval(" + param[0].toString() + ", " + param[1].toString() + ")";
}

function make(lower, upper) {
  if (lower > upper) {
    return [
            upper,
            lower
          ];
  } else {
    return [
            lower,
            upper
          ];
  }
}

function makeWithLength(lower, length) {
  if (length < BigInt(0)) {
    PervasivesU.failwith("length must be non-negative");
  }
  var upper = lower + length - BigInt(1);
  return make(lower, upper);
}

function length(param) {
  var upper = param[1];
  var lower = param[0];
  if (upper > lower) {
    return upper - lower + BigInt(1);
  } else {
    return lower - upper + BigInt(1);
  }
}

function equals(param, param$1) {
  if (param[0] === param$1[0]) {
    return param[1] === param$1[1];
  } else {
    return false;
  }
}

function contains(param, num) {
  if (num >= param[0]) {
    return num <= param[1];
  } else {
    return false;
  }
}

function isOverlap(a, b) {
  if (contains(a, b[0]) || contains(a, b[1]) || contains(b, a[0])) {
    return true;
  } else {
    return contains(b, a[1]);
  }
}

function intersect(a, b) {
  if (isOverlap(a, b)) {
    return make(Stdlib__Math.$$BigInt.max(a[0], b[0]), Stdlib__Math.$$BigInt.min(a[1], b[1]));
  }
  
}

function below(a, b) {
  return a[1] < b[0];
}

function adjacent(a, b) {
  if (a[1] + 1n === b[0]) {
    return true;
  } else {
    return b[1] + 1n === a[0];
  }
}

function belowAndAdjacent(a, b) {
  if (below(a, b)) {
    return adjacent(a, b);
  } else {
    return false;
  }
}

function add(param, num) {
  return make(param[0] + num, param[1] + num);
}

function remove(a, b) {
  var bUpper = b[1];
  var bLower = b[0];
  var aUpper = a[1];
  var aLower = a[0];
  var match = isOverlap(a, b);
  var match$1 = equals(a, b);
  var match$2 = bUpper >= aUpper && contains(a, bLower);
  var match$3 = bLower <= aLower && contains(a, bUpper);
  if (match) {
    if (match$1) {
      return ;
    } else if (match$2) {
      return [
              aLower,
              bLower - 1n
            ];
    } else if (match$3) {
      return [
              bUpper + 1n,
              aUpper
            ];
    } else {
      return ;
    }
  } else {
    return a;
  }
}

function merge(a, b) {
  if (!(adjacent(a, b) || isOverlap(a, b))) {
    return PervasivesU.failwith("intervals must be adjacent or overlapping");
  }
  var lower = Stdlib__Math.$$BigInt.min(a[0], b[0]);
  var upper = Stdlib__Math.$$BigInt.max(a[1], b[1]);
  return make(lower, upper);
}

function sort(intervals) {
  var lowerBoundAscendingCmp = function (param, param$1) {
    var bUpper = param$1[1];
    var bLower = param$1[0];
    var aUpper = param[1];
    var aLower = param[0];
    if (aLower < bLower) {
      return -1;
    } else if (aLower > bLower) {
      return 1;
    } else if (aUpper < bUpper) {
      return -1;
    } else if (aUpper > bUpper) {
      return 1;
    } else {
      return 0;
    }
  };
  return intervals.toSorted(lowerBoundAscendingCmp);
}

function sortAndMergeOverlaps(intervals) {
  var sortedIntervals = sort(intervals);
  var loop = function (_sortedIntervals) {
    while(true) {
      var sortedIntervals = _sortedIntervals;
      var len = sortedIntervals.length;
      if (len === 1) {
        return [sortedIntervals[0]];
      }
      if (len === 0) {
        return [];
      }
      var a = Stdlib__Array.getUnsafe(sortedIntervals, 0);
      var b = Stdlib__Array.getUnsafe(sortedIntervals, 1);
      if (below(a, b) && !adjacent(a, b)) {
        return Belt_Array.concatMany([
                    [a],
                    loop(sortedIntervals.slice(1))
                  ]);
      }
      _sortedIntervals = Belt_Array.concatMany([
            [merge(a, b)],
            sortedIntervals.slice(2)
          ]);
      continue ;
    };
  };
  return loop(sortedIntervals);
}

var $$BigInt$1;

export {
  $$BigInt$1 as $$BigInt,
  log ,
  log2 ,
  toString ,
  make ,
  makeWithLength ,
  length ,
  equals ,
  contains ,
  isOverlap ,
  intersect ,
  below ,
  adjacent ,
  belowAndAdjacent ,
  add ,
  remove ,
  merge ,
  sort ,
  sortAndMergeOverlaps ,
}
/* Stdlib__Array Not a pure module */
