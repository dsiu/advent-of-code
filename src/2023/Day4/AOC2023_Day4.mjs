// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Core__Int from "@rescript/core/src/Core__Int.mjs";
import * as Core__Array from "@rescript/core/src/Core__Array.mjs";
import * as Core__Option from "@rescript/core/src/Core__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(function (l) {
              var cardAndNumbers = l.trim().split(": ");
              var cardId = Core__Option.flatMap(cardAndNumbers[0], (function (s) {
                      return Core__Int.fromString(undefined, s.replace("Card ", ""));
                    }));
              var numberStrs = Core__Option.flatMap(cardAndNumbers[1], (function (s) {
                      return s.split(" | ").map(function (nums) {
                                  var partial_arg = 10;
                                  return Core__Array.filterMap(nums.trim().split(" "), (function (param) {
                                                return Core__Int.fromString(partial_arg, param);
                                              }));
                                });
                    }));
              return {
                      id: Core__Option.getExn(cardId),
                      winners: Core__Option.getExn(Core__Option.flatMap(numberStrs, (function (__x) {
                                  return __x.at(0);
                                }))),
                      actuals: Core__Option.getExn(Core__Option.flatMap(numberStrs, (function (__x) {
                                  return __x.at(1);
                                })))
                    };
            });
}

function solvePart1(data) {
  var prim = parse(data);
  console.log(prim);
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
