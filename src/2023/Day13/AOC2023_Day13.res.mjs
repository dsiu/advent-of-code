// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.res.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.res.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.res.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function diffArray(a, b) {
  return Stdlib__Array.zipWith(a, b, (prim0, prim1) => prim0 === prim1).filter(x => Stdlib__Function.id(!x)).length;
}

function diffStrings(a, b) {
  return Stdlib__Array.zipWith(a.split(""), b.split(""), (prim0, prim1) => prim0 === prim1).filter(x => Stdlib__Function.id(!x)).length;
}

function reflectAt(xs, n, nDiff) {
  let match = Stdlib__Array.splitAt(xs, n);
  return Stdlib__Array.sum(Stdlib__Array.zipWith(match[0].toReversed(), match[1], diffStrings), {
    zero: Stdlib__Int.zero,
    add: Stdlib__Int.add
  }) === nDiff;
}

function reflectionLines(xs, nDiff) {
  let k = xs.length;
  return Stdlib__Array.range(1, k).filter(x => reflectAt(xs, x, nDiff))[0];
}

function transposeArrayOfString(xs) {
  let y = Stdlib__Array.transpose(xs.map(__x => __x.split("")));
  return y.map(x => Stdlib__Array.foldl1(x, (prim0, prim1) => prim0.concat(prim1)));
}

function reflections(patt, nDiff) {
  let hline = Stdlib__Option.map(reflectionLines(patt, nDiff), x => ({
    TAG: "Horiz",
    _0: x
  }));
  let vline = Stdlib__Option.map(reflectionLines(transposeArrayOfString(patt), nDiff), x => ({
    TAG: "Vert",
    _0: x
  }));
  return Stdlib__Option.getExn(Stdlib__Option.orElse(hline, vline), undefined);
}

function score(l) {
  if (l.TAG === "Horiz") {
    return Math.imul(100, l._0);
  } else {
    return l._0;
  }
}

function solve(xs, nDiff) {
  return Stdlib__Array.sum(xs.map((x, _i) => reflections(x, nDiff)).map(score), {
    zero: Stdlib__Int.zero,
    add: Stdlib__Int.add
  });
}

function part1(xs) {
  return solve(xs, 0);
}

function part2(xs) {
  return solve(xs, 1);
}

function parse(data) {
  return Utils.splitDoubleNewline(data).map(lines => Utils.splitNewline(lines).map(prim => prim.trim()));
}

function solvePart1(data) {
  return solve(parse(data), 0);
}

function solvePart2(data) {
  return solve(parse(data), 1);
}

export {
  log,
  log2,
  diffArray,
  diffStrings,
  reflectAt,
  reflectionLines,
  transposeArrayOfString,
  reflections,
  score,
  solve,
  part1,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */