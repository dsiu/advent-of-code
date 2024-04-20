// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_SetInt from "rescript/lib/es6/belt_SetInt.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function matchPerCard(param) {
  return Belt_SetInt.size(Belt_SetInt.intersect(Belt_SetInt.fromArray(param.winners), Belt_SetInt.fromArray(param.actuals)));
}

function mkQueue(cards) {
  return cards.map(function (c) {
              return {
                      numMatches: matchPerCard(c),
                      queuedQuantity: 1
                    };
            });
}

function duplicateCards(n, scale, queue) {
  var qPre = queue.slice(0, n);
  var qPost = queue.slice(n);
  var dup = qPre.map(function (param) {
        return {
                numMatches: param.numMatches,
                queuedQuantity: param.queuedQuantity + scale | 0
              };
      });
  return dup.concat(qPost);
}

function calculatePoint(n) {
  if (n === 0) {
    return 0;
  } else {
    return Math.pow(2, n - 1 | 0);
  }
}

function part1(cards) {
  return Utils$AdventOfCode.sumIntArray(cards.map(Utils$AdventOfCode.compose(matchPerCard, calculatePoint)));
}

function part2(cards) {
  var queue = mkQueue(cards);
  var _n = 0;
  var _q = queue;
  while(true) {
    var q = _q;
    var n = _n;
    var match = q.length;
    if (match === 0) {
      return n;
    }
    var match$1 = Stdlib__Option.getExn(q[0], undefined);
    var queuedQuantity = match$1.queuedQuantity;
    var n$p = n + queuedQuantity | 0;
    var queue$p = duplicateCards(match$1.numMatches, queuedQuantity, q.slice(1));
    _q = queue$p;
    _n = n$p;
    continue ;
  };
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(function (l) {
              var match = l.trim().split(": ");
              if (match.length !== 2) {
                throw {
                      RE_EXN_ID: "Match_failure",
                      _1: [
                        "AOC2023_Day4.res",
                        163,
                        8
                      ],
                      Error: new Error()
                    };
              }
              var cardIdStr = match[0];
              var numberStrs = match[1];
              var cardId = Stdlib__Option.getExn(Stdlib__Int.fromString(cardIdStr.replace("Card ", ""), undefined), undefined);
              var match$1 = numberStrs.split(" | ");
              if (match$1.length !== 2) {
                throw {
                      RE_EXN_ID: "Match_failure",
                      _1: [
                        "AOC2023_Day4.res",
                        165,
                        8
                      ],
                      Error: new Error()
                    };
              }
              var winnersStr = match$1[0];
              var actualsStr = match$1[1];
              var winners = Stdlib__Array.filterMap(winnersStr.split(" "), (function (none) {
                      return Stdlib__Int.fromString(none, 10);
                    }));
              var actuals = Stdlib__Array.filterMap(actualsStr.split(" "), (function (none) {
                      return Stdlib__Int.fromString(none, 10);
                    }));
              return {
                      id: cardId,
                      winners: winners,
                      actuals: actuals
                    };
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
  log2 ,
  matchPerCard ,
  mkQueue ,
  duplicateCards ,
  calculatePoint ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Int Not a pure module */
