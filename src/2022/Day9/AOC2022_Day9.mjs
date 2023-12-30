// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Stdlib_Array from "@dsiu/rescript-stdlib-fp/src/Stdlib_Array.mjs";
import * as TableclothSet from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothSet.mjs";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Coord_V2$AdventOfCode from "../../Coord_V2.mjs";

function log(prim) {
  console.log(prim);
}

var emptyPositionSet = TableclothSet.empty({
      comparator: Coord_V2$AdventOfCode.comparator
    });

function newRope(n) {
  return {
          TAG: "Rope",
          headK: [
            0,
            0
          ],
          knots: Stdlib_Array.make(n, [
                0,
                0
              ]),
          trace: TableclothSet.add(emptyPositionSet, [
                0,
                0
              ])
        };
}

var ParseError = /* @__PURE__ */Caml_exceptions.create("AOC2022_Day9-AdventOfCode.ParseError");

function expandPath(directions) {
  var expandStep = function (step) {
    switch (step.TAG) {
      case "U" :
          return Stdlib_Array.make(step._0, [
                      0,
                      1
                    ]);
      case "R" :
          return Stdlib_Array.make(step._0, [
                      1,
                      0
                    ]);
      case "D" :
          return Stdlib_Array.make(step._0, [
                      0,
                      -1
                    ]);
      case "L" :
          return Stdlib_Array.make(step._0, [
                      -1,
                      0
                    ]);
      
    }
  };
  return Stdlib_Array.flatMap(directions, expandStep);
}

function manhattan(p1, p2) {
  return Caml.int_max(Pervasives.abs(p1[0] - p2[0] | 0), Pervasives.abs(p1[1] - p2[1] | 0));
}

function touching(p1, p2) {
  return manhattan(p1, p2) <= 1;
}

function sign(n) {
  return Caml.int_compare(n, 0);
}

function towards(p1, p2) {
  return [
          Caml.int_compare(p2[0] - p1[0] | 0, 0),
          Caml.int_compare(p2[1] - p1[1] | 0, 0)
        ];
}

function knotStep(param, kt) {
  var h = param[0];
  var kt$p = touching(kt, h) ? kt : Coord_V2$AdventOfCode.add(kt, towards(kt, h));
  return [
          kt$p,
          [kt$p].concat(param[1])
        ];
}

function ropeStep(rope, step) {
  var h = Coord_V2$AdventOfCode.add(rope.headK, step);
  var match = Stdlib_Array.reduce(rope.knots, [
        h,
        []
      ], knotStep);
  return {
          TAG: "Rope",
          headK: h,
          knots: match[1].toReversed(),
          trace: TableclothSet.add(rope.trace, match[0])
        };
}

function ropeSteps(rope, steps) {
  return Stdlib_Array.reduce(steps, rope, ropeStep);
}

function part1(steps) {
  var rope = Stdlib_Array.reduce(steps, newRope(1), ropeStep);
  return TableclothSet.length(rope.trace);
}

function part2(steps) {
  var rope = Stdlib_Array.reduce(steps, newRope(9), ropeStep);
  return TableclothSet.length(rope.trace);
}

function parse(data) {
  return Stdlib_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                var match = x.trim().split(" ");
                if (match.length !== 2) {
                  throw {
                        RE_EXN_ID: "Match_failure",
                        _1: [
                          "AOC2022_Day9.res",
                          99,
                          8
                        ],
                        Error: new Error()
                      };
                }
                var dStr = match[0];
                var steps = match[1];
                var match$1 = Caml_format.int_of_string(steps);
                switch (dStr) {
                  case "D" :
                      return {
                              TAG: "D",
                              _0: match$1
                            };
                  case "L" :
                      return {
                              TAG: "L",
                              _0: match$1
                            };
                  case "R" :
                      return {
                              TAG: "R",
                              _0: match$1
                            };
                  case "U" :
                      return {
                              TAG: "U",
                              _0: match$1
                            };
                  default:
                    throw {
                          RE_EXN_ID: ParseError,
                          _1: dStr + steps,
                          Error: new Error()
                        };
                }
              }));
}

function solvePart1(data) {
  return part1(expandPath(parse(data)));
}

function solvePart2(data) {
  return part2(expandPath(parse(data)));
}

var A;

var TC;

var Position;

export {
  log ,
  A ,
  TC ,
  Position ,
  emptyPositionSet ,
  newRope ,
  ParseError ,
  expandPath ,
  manhattan ,
  touching ,
  sign ,
  towards ,
  knotStep ,
  ropeStep ,
  ropeSteps ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* emptyPositionSet Not a pure module */
