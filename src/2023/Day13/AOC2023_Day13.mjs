// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Stdlib__String from "@dsiu/rescript-stdlib-fp/src/Stdlib__String.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var zipWith = Stdlib__Array.map2;

function reflectAt(xs, n) {
  var match = Stdlib__Array.splitAt(xs, n);
  var t = match[1];
  var h = match[0];
  Stdlib__Array.map2(h.toReversed(), t, Stdlib__String.equal);
  return Stdlib__Array.all(Stdlib__Array.map2(h.toReversed(), t, Stdlib__String.equal), Stdlib__Function.id);
}

function reflectionLines(xs) {
  var k = xs.length;
  return Stdlib__Array.filter(Stdlib__Array.range(1, k), (function (x) {
                  return reflectAt(xs, x);
                }))[0];
}

function transposeArrayOfString(xs) {
  var y = Stdlib__Array.transpose(Stdlib__Array.map(xs, (function (__x) {
              return __x.split("");
            })));
  return Stdlib__Array.map(y, (function (x) {
                return Stdlib__Array.foldl1(x, (function (prim0, prim1) {
                              return prim0.concat(prim1);
                            }));
              }));
}

function reflections(patt) {
  var hline = Stdlib__Option.map(reflectionLines(patt), (function (x) {
          return {
                  TAG: "Horiz",
                  _0: x
                };
        }));
  var vline = Stdlib__Option.map(reflectionLines(transposeArrayOfString(patt)), (function (x) {
          return {
                  TAG: "Vert",
                  _0: x
                };
        }));
  return Stdlib__Option.getExn(Stdlib__Option.orElse(hline, vline));
}

function score(l) {
  if (l.TAG === "Horiz") {
    return Math.imul(100, l._0);
  } else {
    return l._0;
  }
}

function part1(xs) {
  return Stdlib__Array.sum(Stdlib__Array.map(Stdlib__Array.mapWithIndex(xs, (function (x, i) {
                        return reflections(x);
                      })), score), {
              zero: Stdlib__Int.zero,
              add: Stdlib__Int.add
            });
}

function parse(data) {
  return Stdlib__Array.map(Utils$AdventOfCode.splitDoubleNewline(data), (function (lines) {
                return Stdlib__Array.map(Utils$AdventOfCode.splitNewline(lines), (function (prim) {
                              return prim.trim();
                            }));
              }));
}

function solvePart1(data) {
  var prim = part1(parse(data));
  console.log(prim);
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log ,
  log2 ,
  zipWith ,
  reflectAt ,
  reflectionLines ,
  transposeArrayOfString ,
  reflections ,
  score ,
  part1 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Int Not a pure module */
