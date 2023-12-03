// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Stdlib_Array from "@dsiu/rescript-stdlib-fp/src/Stdlib_Array.mjs";
import * as Stdlib_Option from "@dsiu/rescript-stdlib-fp/src/Stdlib_Option.mjs";
import * as Stdlib_Function from "@dsiu/rescript-stdlib-fp/src/Stdlib_Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function log(prim) {
  console.log(prim);
}

function extractName(c) {
  return c._0;
}

function getCratesForWharf(crates, wharf) {
  var idx = ((wharf - 1 | 0) << 2) + 1 | 0;
  return Stdlib_Array.map(crates, (function (x) {
                return Stdlib_Option.flatMap(Stdlib_Array.get(x, idx), (function (x) {
                              if (x === " ") {
                                return ;
                              } else {
                                return {
                                        TAG: "Crate",
                                        _0: x
                                      };
                              }
                            }));
              }));
}

function catMaybes(__x) {
  return Stdlib_Array.keepMap(__x, Stdlib_Function.identity);
}

function makeWharf(wharfLines, colNames) {
  return Stdlib_Array.reduce(colNames, undefined, (function (acc, colName) {
                var __x = getCratesForWharf(wharfLines, colName);
                return Belt_MapInt.set(acc, colName, Stdlib_Array.keepMap(__x, Stdlib_Function.identity));
              }));
}

function makeMoves(xs) {
  return Stdlib_Array.map(xs, (function (x) {
                var parts = x.split(" ");
                return {
                        TAG: "Move",
                        _0: Stdlib_Option.getExn(Stdlib_Option.flatMap(Stdlib_Array.get(parts, 1), Belt_Int.fromString)),
                        _1: Stdlib_Option.getExn(Stdlib_Option.flatMap(Stdlib_Array.get(parts, 3), Belt_Int.fromString)),
                        _2: Stdlib_Option.getExn(Stdlib_Option.flatMap(Stdlib_Array.get(parts, 5), Belt_Int.fromString))
                      };
              }));
}

function parse(data) {
  var text = Stdlib_Array.map(Utils$AdventOfCode.splitDoubleNewline(data), Utils$AdventOfCode.splitNewline);
  var firstSection = Stdlib_Array.tail(Stdlib_Array.getExn(text, 0));
  var secondSection = Stdlib_Array.init(Stdlib_Array.getExn(text, 1));
  var wharfLines = Stdlib_Array.map(Stdlib_Option.getExn(Stdlib_Array.init(firstSection)), Utils$AdventOfCode.splitChars);
  var colNames = Stdlib_Array.keepMap(Stdlib_Array.last(firstSection).split(" "), Belt_Int.fromString);
  var moves = makeMoves(Stdlib_Option.getExn(secondSection));
  var wharf = makeWharf(wharfLines, colNames);
  return [
          wharf,
          moves
        ];
}

function makeMove1(wharf, param) {
  var to_ = param._2;
  var from = param._1;
  var f = Belt_MapInt.getExn(wharf, from);
  var c = Stdlib_Array.head(f);
  var origin = Stdlib_Array.tail(f);
  var dest = Stdlib_Array.append([c], Belt_MapInt.getExn(wharf, to_));
  return Belt_MapInt.set(Belt_MapInt.set(wharf, to_, dest), from, origin);
}

function applyMove1(wharf, m) {
  return Stdlib_Array.reduce(Stdlib_Array.makeBy(m._0, (function (param) {
                    return m;
                  })), wharf, makeMove1);
}

function applyMoves1(wharf, moves) {
  return Stdlib_Array.reduce(moves, wharf, applyMove1);
}

function applyMove2(wharf, param) {
  var to_ = param._2;
  var from = param._1;
  var n = param._0;
  var origin = Belt_MapInt.getExn(wharf, from);
  var moving = Stdlib_Array.take(origin, n);
  var origin$p = Stdlib_Array.drop(origin, n);
  var dest = Stdlib_Array.append(moving, Belt_MapInt.getExn(wharf, to_));
  return Belt_MapInt.set(Belt_MapInt.set(wharf, to_, dest), from, origin$p);
}

function applyMoves2(wharf, moves) {
  return Stdlib_Array.reduce(moves, wharf, applyMove2);
}

function showTops(wharf) {
  return Stdlib_Array.foldLeft(Stdlib_Array.map(Belt_MapInt.valuesToArray(wharf), (function (param) {
                    return Utils$AdventOfCode.compose(Stdlib_Array.head, extractName, param);
                  })), (function (prim0, prim1) {
                return prim0.concat(prim1);
              }));
}

function solvePart1(data) {
  var match = parse(data);
  return showTops(Stdlib_Array.reduce(match[1], match[0], applyMove1));
}

function solvePart2(data) {
  var match = parse(data);
  return showTops(Stdlib_Array.reduce(match[1], match[0], applyMove2));
}

var A;

var S;

var O;

var M;

export {
  A ,
  S ,
  O ,
  M ,
  log2 ,
  log ,
  extractName ,
  getCratesForWharf ,
  catMaybes ,
  makeWharf ,
  makeMoves ,
  parse ,
  makeMove1 ,
  applyMove1 ,
  applyMoves1 ,
  applyMove2 ,
  applyMoves2 ,
  showTops ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
