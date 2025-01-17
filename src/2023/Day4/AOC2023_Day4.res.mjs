// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Belt_SetInt from "rescript/lib/es6/Belt_SetInt.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.res.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.res.mjs";

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
  return cards.map(c => ({
    numMatches: matchPerCard(c),
    queuedQuantity: 1
  }));
}

function duplicateCards(n, scale, queue) {
  let qPre = queue.slice(0, n);
  let qPost = queue.slice(n);
  let dup = qPre.map(param => ({
    numMatches: param.numMatches,
    queuedQuantity: param.queuedQuantity + scale | 0
  }));
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
  return Utils.sumIntArray(cards.map(Utils.compose(matchPerCard, calculatePoint)));
}

function part2(cards) {
  let queue = mkQueue(cards);
  let _n = 0;
  let _q = queue;
  while (true) {
    let q = _q;
    let n = _n;
    let match = q.length;
    if (match === 0) {
      return n;
    }
    let match$1 = Stdlib__Option.getExn(q[0], undefined);
    let queuedQuantity = match$1.queuedQuantity;
    let n$p = n + queuedQuantity | 0;
    let queue$p = duplicateCards(match$1.numMatches, queuedQuantity, q.slice(1));
    _q = queue$p;
    _n = n$p;
    continue;
  };
}

function parse(data) {
  return Utils.splitNewline(data).map(l => {
    let match = l.trim().split(": ");
    if (match.length !== 2) {
      throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2023_Day4.res",
          164,
          8
        ],
        Error: new Error()
      };
    }
    let cardIdStr = match[0];
    let numberStrs = match[1];
    let cardId = Stdlib__Option.getExn(Stdlib__Int.fromString(cardIdStr.replace("Card ", ""), undefined), undefined);
    let match$1 = numberStrs.split(" | ");
    if (match$1.length !== 2) {
      throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2023_Day4.res",
          166,
          8
        ],
        Error: new Error()
      };
    }
    let winnersStr = match$1[0];
    let actualsStr = match$1[1];
    let winners = Stdlib__Array.filterMap(winnersStr.split(" "), none => Stdlib__Int.fromString(none, 10));
    let actuals = Stdlib__Array.filterMap(actualsStr.split(" "), none => Stdlib__Int.fromString(none, 10));
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
  log,
  log2,
  matchPerCard,
  mkQueue,
  duplicateCards,
  calculatePoint,
  part1,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */