// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function diffArray(a, b) {
  return Stdlib__Array.zipWith(a, b, (function (prim0, prim1) {
                  return prim0 === prim1;
                })).filter(function (x) {
              return Stdlib__Function.id(!x);
            }).length;
}

function diffStrings(a, b) {
  return Stdlib__Array.zipWith(a.split(""), b.split(""), (function (prim0, prim1) {
                  return prim0 === prim1;
                })).filter(function (x) {
              return Stdlib__Function.id(!x);
            }).length;
}

function reflectAt(xs, n, nDiff) {
  var match = Stdlib__Array.splitAt(xs, n);
  return Stdlib__Array.sum(Stdlib__Array.zipWith(match[0].toReversed(), match[1], diffStrings), {
              zero: Stdlib__Int.zero,
              add: Stdlib__Int.add
            }) === nDiff;
}

function reflectionLines(xs, nDiff) {
  var k = xs.length;
  return Stdlib__Array.range(1, k).filter(function (x) {
                return reflectAt(xs, x, nDiff);
              })[0];
}

function transposeArrayOfString(xs) {
  var y = Stdlib__Array.transpose(xs.map(function (__x) {
            return __x.split("");
          }));
  return y.map(function (x) {
              return Stdlib__Array.foldl1(x, (function (prim0, prim1) {
                            return prim0.concat(prim1);
                          }));
            });
}

function reflections(patt, nDiff) {
  var hline = Stdlib__Option.map(reflectionLines(patt, nDiff), (function (x) {
          return {
                  TAG: "Horiz",
                  _0: x
                };
        }));
  var vline = Stdlib__Option.map(reflectionLines(transposeArrayOfString(patt), nDiff), (function (x) {
          return {
                  TAG: "Vert",
                  _0: x
                };
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
  return Stdlib__Array.sum(xs.map(function (x, i) {
                    return reflections(x, nDiff);
                  }).map(score), {
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
  return Utils$AdventOfCode.splitDoubleNewline(data).map(function (lines) {
              return Utils$AdventOfCode.splitNewline(lines).map(function (prim) {
                          return prim.trim();
                        });
            });
}

function solvePart1(data) {
  return solve(parse(data), 0);
}

function solvePart2(data) {
  return solve(parse(data), 1);
}

export {
  log ,
  log2 ,
  diffArray ,
  diffStrings ,
  reflectAt ,
  reflectionLines ,
  transposeArrayOfString ,
  reflections ,
  score ,
  solve ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Int Not a pure module */
