// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as ChineseRemainder$AdventOfCode from "../../ChineseRemainder.mjs";

function log(prim) {
  console.log(prim);
}

function findEarliestBus(param) {
  var time_stamp = param[0];
  return Belt_Array.reduce(Belt_Array.map(param[1], (function (x) {
                    return [
                            x,
                            x - Caml_int32.mod_(time_stamp, x) | 0
                          ];
                  })), [
              0,
              10000
            ], (function (param, param$1) {
                var min = param$1[1];
                var amin = param[1];
                if (min < amin) {
                  return [
                          param$1[0],
                          min
                        ];
                } else {
                  return [
                          param[0],
                          amin
                        ];
                }
              }));
}

function parse(data) {
  var lines = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
          return x.trim();
        }));
  var timeStamp = Utils$AdventOfCode.intFromStringExn(Belt_Option.getExn(Belt_Array.get(lines, 0)));
  var isNotX = function (c) {
    return c !== "x";
  };
  var bus_numbers = Belt_Array.map(Belt_Array.keep(Belt_Option.getExn(Belt_Array.get(lines, 1)).split(","), isNotX), Utils$AdventOfCode.intFromStringExn);
  return [
          timeStamp,
          bus_numbers
        ];
}

function solvePart2BruteForce(xs) {
  var big_zero = BigInt(0);
  var big_one = BigInt(1);
  var _time = BigInt("100000000000000");
  while(true) {
    var time = _time;
    if (Belt_Array.reduceU(xs, true, (function(time){
          return function (acc, param) {
            if (acc) {
              return (time + param[1]) % param[0] === big_zero;
            } else {
              return false;
            }
          }
          }(time)))) {
      return time;
    }
    _time = time + big_one;
    continue ;
  };
}

function part2(xs) {
  var rem = {
    contents: []
  };
  var num = {
    contents: []
  };
  Belt_Array.forEach(xs, (function (param) {
          var bus = param[0];
          rem.contents = Belt_Array.concat(rem.contents, [bus - param[1]]);
          num.contents = Belt_Array.concat(num.contents, [bus]);
        }));
  return ChineseRemainder$AdventOfCode.crtBigInt(rem.contents, num.contents);
}

function parse2(data) {
  var lines = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
          return x.trim();
        }));
  return Belt_Array.reduceWithIndex(Belt_Option.getExn(Belt_Array.get(lines, 1)).split(","), [], (function (acc, x, i) {
                if (x !== "x") {
                  return Belt_Array.concat(acc, [[
                                BigInt(x),
                                BigInt(i)
                              ]]);
                } else {
                  return acc;
                }
              }));
}

function solvePart1(data) {
  var match = findEarliestBus(parse(data));
  return Math.imul(match[0], match[1]);
}

function solvePart2(data) {
  return part2(parse2(data));
}

export {
  log ,
  findEarliestBus ,
  parse ,
  solvePart2BruteForce ,
  part2 ,
  parse2 ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
