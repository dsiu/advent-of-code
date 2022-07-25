// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_SortArrayInt from "rescript/lib/es6/belt_SortArrayInt.js";
import * as Belt_MutableMapInt from "rescript/lib/es6/belt_MutableMapInt.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function diffs(xs, ys) {
  return Belt_Array.mapWithIndex(xs, (function (i, x) {
                return Belt_Array.getExn(ys, i) - x | 0;
              }));
}

function tally(xs) {
  var max = Utils$AdventOfCode.maxIntInArray(xs);
  var min = Utils$AdventOfCode.minIntInArray(xs);
  var keyRange = Belt_Array.range(min, max);
  var result = Belt_MutableMapInt.make(undefined);
  Belt_Array.forEach(keyRange, (function (k) {
          var match = Belt_Array.partition(xs, (function (x) {
                  return x === k;
                }));
          var n = match[0].length;
          if (n > 0) {
            return Belt_MutableMapInt.set(result, k, n);
          }
          
        }));
  return result;
}

function groupOnes(xs) {
  var helper = function (_arr, _res) {
    while(true) {
      var res = _res;
      var arr = _arr;
      var threeIdx = Belt_Array.getIndexBy(arr, (function (x) {
              return x === 3;
            }));
      if (threeIdx === undefined) {
        return res;
      }
      var res$1 = Belt_Array.concat(res, [threeIdx]);
      if ((threeIdx + 1 | 0) > arr.length) {
        return res$1;
      }
      _res = res$1;
      _arr = Belt_Array.sliceToEnd(arr, threeIdx + 1 | 0);
      continue ;
    };
  };
  return Belt_Array.keep(helper(xs, []), (function (x) {
                return x > 0;
              }));
}

function convertToMultiplier(xs) {
  return Belt_Array.map(xs, (function (x) {
                switch (x) {
                  case 2 :
                      return 2;
                  case 3 :
                      return 4;
                  case 4 :
                      return 7;
                  case 5 :
                      return 11;
                  default:
                    return x;
                }
              }));
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return Belt_Option.getExn(Belt_Int.fromString(x.trim()));
              }));
}

function solvePart1(data) {
  var adapters = Belt_SortArrayInt.stableSort(parse(data));
  var a = Belt_Array.concat([0], adapters);
  var b = Belt_Array.concat(adapters, [Utils$AdventOfCode.maxIntInArray(adapters) + 3 | 0]);
  var diffs$1 = diffs(a, b);
  var tally$1 = tally(diffs$1);
  return Math.imul(Belt_MutableMapInt.getExn(tally$1, 1), Belt_MutableMapInt.getExn(tally$1, 3));
}

function solvePart2(data) {
  var adapters = Belt_SortArrayInt.stableSort(parse(data));
  var a = Belt_Array.concat([0], adapters);
  var b = Belt_Array.concat(adapters, [Utils$AdventOfCode.maxIntInArray(adapters) + 3 | 0]);
  var diffs$1 = diffs(a, b);
  var ones = groupOnes(diffs$1);
  var multiplers = convertToMultiplier(ones);
  return Belt_Array.reduce(multiplers, 1.0, (function (acc, x) {
                return x * acc;
              }));
}

export {
  log ,
  log2 ,
  diffs ,
  tally ,
  groupOnes ,
  convertToMultiplier ,
  parse ,
  solvePart1 ,
  solvePart2 ,
  
}
/* No side effect */