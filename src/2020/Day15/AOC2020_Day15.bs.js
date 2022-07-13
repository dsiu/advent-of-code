// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as JS_Debug$AdventOfCode from "../../JS_Debug.bs.js";

function log(prim) {
  console.log(prim);
  
}

var Log = JS_Debug$AdventOfCode.make("AOC2020_Day15-AdventOfCode");

function get_last_2_diff(t) {
  var len = t.length;
  if (len === 0 || len === 1) {
    return 0;
  } else {
    return t[len - 1 | 0] - t[len - 2 | 0] | 0;
  }
}

function append(t, x) {
  var len = t.length;
  if (len !== 0) {
    if (len !== 1) {
      return [
              t[len - 1 | 0],
              x
            ];
    } else {
      return [
              t[0],
              x
            ];
    }
  } else {
    return [x];
  }
}

var Appear = {
  get_last_2_diff: get_last_2_diff,
  append: append
};

function solve(xs, total_turn) {
  var spoken = Belt_Array.reduceWithIndexU(xs, undefined, (function (a, x, i) {
          return Belt_MapInt.set(a, x, [i + 1 | 0]);
        }));
  var len = xs.length;
  var _spoken = spoken;
  var _last_num = Belt_Array.getExn(xs, len - 1 | 0);
  var _this_turn = len + 1 | 0;
  while(true) {
    var this_turn = _this_turn;
    var last_num = _last_num;
    var spoken$1 = _spoken;
    Curry._1(Log.debug, String(this_turn));
    if (this_turn > total_turn) {
      return last_num;
    }
    var last_num_appear = Belt_MapInt.get(spoken$1, last_num);
    var next_num = Belt_Option.getExn(Belt_Option.flatMap(last_num_appear, get_last_2_diff));
    var next_spoken = Belt_MapInt.update(spoken$1, next_num, (function(this_turn){
        return function (next_num_appear) {
          if (next_num_appear !== undefined) {
            return append(next_num_appear, this_turn);
          } else {
            return [this_turn];
          }
        }
        }(this_turn)));
    _this_turn = this_turn + 1 | 0;
    _last_num = next_num;
    _spoken = next_spoken;
    continue ;
  };
}

function parse(data) {
  return Belt_Array.map(data.trim().split(","), (function (x) {
                return Belt_Option.getExn(Belt_Int.fromString(x));
              }));
}

function solvePart1(data) {
  return solve(parse(data), 2020);
}

function solvePart2(data) {
  return solve(parse(data), 30000000);
}

export {
  log ,
  Log ,
  Appear ,
  solve ,
  parse ,
  solvePart1 ,
  solvePart2 ,
  
}
/* Log Not a pure module */
