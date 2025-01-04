// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function extendOne(partials, next) {
  let go = p => [
    p + next,
    p * next
  ];
  return partials.flatMap(go);
}

function extend(arr) {
  let x = Stdlib__Array.headUnsafe(arr);
  let xs = Stdlib__Array.tail(arr);
  return Stdlib__Array.reduce(xs, [x], extendOne);
}

function isValid(param) {
  let target = param[0];
  return extend(param[1]).find(x => x === target);
}

function concatBigInt(a, b) {
  return BigInt(a.toString() + b.toString());
}

function extendOneC(partials, next) {
  let go = p => [
    p + next,
    p * next,
    concatBigInt(p, next)
  ];
  return partials.flatMap(go);
}

function extendC(arr) {
  let x = Stdlib__Array.headUnsafe(arr);
  let xs = Stdlib__Array.tail(arr);
  return Stdlib__Array.reduce(xs, [x], extendOneC);
}

function isValidC(param) {
  let target = param[0];
  return extendC(param[1]).find(x => x === target);
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(l => {
    let arr = l.trim().split(": ");
    if (arr.length !== 2) {
      throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2024_Day7.res",
          48,
          8
        ],
        Error: new Error()
      };
    }
    let first = arr[0];
    let second = arr[1];
    return [
      BigInt(first),
      second.split(" ").map(prim => BigInt(prim))
    ];
  });
}

function sumBigIntArray(__x) {
  return Stdlib__Array.reduce(__x, 0n, (a, b) => a + b);
}

function solvePart1(data) {
  return sumBigIntArray(Stdlib__Array.keepSome(parse(data).map(isValid)));
}

function solvePart2(data) {
  return sumBigIntArray(Stdlib__Array.keepSome(parse(data).map(isValidC)));
}

export {
  log,
  log2,
  extendOne,
  extend,
  isValid,
  concatBigInt,
  extendOneC,
  extendC,
  isValidC,
  parse,
  sumBigIntArray,
  solvePart1,
  solvePart2,
}
/* Stdlib__Array Not a pure module */
