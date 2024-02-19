// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__String from "@dsiu/rescript-stdlib-fp/src/Stdlib__String.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var numberNames = [
  [
    "zero",
    "0"
  ],
  [
    "one",
    "1"
  ],
  [
    "two",
    "2"
  ],
  [
    "three",
    "3"
  ],
  [
    "four",
    "4"
  ],
  [
    "five",
    "5"
  ],
  [
    "six",
    "6"
  ],
  [
    "seven",
    "7"
  ],
  [
    "eight",
    "8"
  ],
  [
    "nine",
    "9"
  ]
];

function spelledOutToDigits(str, matchDir) {
  var tmp;
  tmp = matchDir === "Left2Right" ? str : Stdlib__String.reverse(str);
  return Belt_Array.reduce(Utils$AdventOfCode.splitChars(tmp), "", (function (acc, c) {
                var tmp;
                tmp = matchDir === "Left2Right" ? acc + c : c + acc;
                return Belt_Array.reduce(numberNames, tmp, (function (curStr, param) {
                              return curStr.replace(param[0], param[1]);
                            }));
              }));
}

function get2Digits(str) {
  var digits = Stdlib__Array.filterMap(Utils$AdventOfCode.splitChars(str), (function (extra) {
          return Stdlib__Int.fromString(10, extra);
        }));
  var first = digits.at(0);
  var last = digits.at(-1);
  return [
          first,
          last
        ];
}

function combineFirstAndLast(param) {
  return Math.imul(param[0], 10) + param[1] | 0;
}

function part2(xs) {
  return Utils$AdventOfCode.sumIntArray(xs.map(function (x) {
                  var match = get2Digits(spelledOutToDigits(x, "Left2Right"));
                  var match$1 = get2Digits(spelledOutToDigits(x, "Right2Left"));
                  return combineFirstAndLast([
                              match[0],
                              match$1[1]
                            ]);
                }));
}

function part1(xs) {
  var result = xs.map(Utils$AdventOfCode.compose(get2Digits, combineFirstAndLast));
  return Utils$AdventOfCode.sumIntArray(result);
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(function (prim) {
              return prim.trim();
            });
}

function solvePart1(data) {
  return part1(parse(data));
}

function solvePart2(data) {
  return part2(parse(data));
}

export {
  log ,
  numberNames ,
  spelledOutToDigits ,
  get2Digits ,
  combineFirstAndLast ,
  part2 ,
  part1 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Int Not a pure module */
