// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/Belt_Int.js";
import * as Belt_MapInt from "rescript/lib/es6/Belt_MapInt.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
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
  let idx = ((wharf - 1 | 0) << 2) + 1 | 0;
  return crates.map(x => Stdlib__Option.flatMap(x[idx], x => {
    if (x === " ") {
      return;
    } else {
      return {
        TAG: "Crate",
        _0: x
      };
    }
  }));
}

function catMaybes(__x) {
  return Stdlib__Array.filterMap(__x, prim => prim);
}

function makeWharf(wharfLines, colNames) {
  return Stdlib__Array.reduce(colNames, undefined, (acc, colName) => {
    let __x = getCratesForWharf(wharfLines, colName);
    return Belt_MapInt.set(acc, colName, Stdlib__Array.filterMap(__x, prim => prim));
  });
}

function makeMoves(xs) {
  let intFromString = Belt_Int.fromString;
  return xs.map(x => {
    let parts = x.split(" ");
    return {
      TAG: "Move",
      _0: Stdlib__Option.getExn(Stdlib__Option.flatMap(parts[1], intFromString), undefined),
      _1: Stdlib__Option.getExn(Stdlib__Option.flatMap(parts[3], intFromString), undefined),
      _2: Stdlib__Option.getExn(Stdlib__Option.flatMap(parts[5], intFromString), undefined)
    };
  });
}

function parse(data) {
  let text = Utils$AdventOfCode.splitDoubleNewline(data).map(Utils$AdventOfCode.splitNewline);
  let firstSection = Stdlib__Array.tail(Stdlib__Array.getUnsafe(text, 0));
  let secondSection = Stdlib__Array.init(Stdlib__Array.getUnsafe(text, 1));
  let wharfLines = Stdlib__Option.getExn(Stdlib__Array.init(firstSection), undefined).map(Utils$AdventOfCode.splitChars);
  let colNames = Stdlib__Array.filterMap(Stdlib__Option.getExn(Stdlib__Array.last(firstSection), undefined).split(" "), Belt_Int.fromString);
  let moves = makeMoves(Stdlib__Option.getExn(secondSection, undefined));
  let wharf = makeWharf(wharfLines, colNames);
  return [
    wharf,
    moves
  ];
}

function makeMove1(wharf, param) {
  let to_ = param._2;
  let from = param._1;
  let f = Belt_MapInt.getExn(wharf, from);
  let c = Stdlib__Array.headUnsafe(f);
  let origin = Stdlib__Array.tail(f);
  let dest = Stdlib__Array.append([c], Belt_MapInt.getExn(wharf, to_));
  return Belt_MapInt.set(Belt_MapInt.set(wharf, to_, dest), from, origin);
}

function applyMove1(wharf, m) {
  return Stdlib__Array.reduce(Stdlib__Array.makeBy(m._0, param => m), wharf, makeMove1);
}

function applyMoves1(wharf, moves) {
  return Stdlib__Array.reduce(moves, wharf, applyMove1);
}

function applyMove2(wharf, param) {
  let to_ = param._2;
  let from = param._1;
  let n = param._0;
  let origin = Belt_MapInt.getExn(wharf, from);
  let moving = Stdlib__Array.take(origin, n);
  let origin$p = Stdlib__Array.drop(origin, n);
  let dest = Stdlib__Array.append(moving, Belt_MapInt.getExn(wharf, to_));
  return Belt_MapInt.set(Belt_MapInt.set(wharf, to_, dest), from, origin$p);
}

function applyMoves2(wharf, moves) {
  return Stdlib__Array.reduce(moves, wharf, applyMove2);
}

function showTops(wharf) {
  return Stdlib__Array.foldl1(Belt_MapInt.valuesToArray(wharf).map(Utils$AdventOfCode.compose(Stdlib__Array.headUnsafe, extractName)), (prim0, prim1) => prim0.concat(prim1));
}

function solvePart1(data) {
  let match = parse(data);
  return showTops(Stdlib__Array.reduce(match[1], match[0], applyMove1));
}

function solvePart2(data) {
  let match = parse(data);
  return showTops(Stdlib__Array.reduce(match[1], match[0], applyMove2));
}

let A;

let S;

let O;

let M;

export {
  A,
  S,
  O,
  M,
  log2,
  log,
  extractName,
  getCratesForWharf,
  catMaybes,
  makeWharf,
  makeMoves,
  parse,
  makeMove1,
  applyMove1,
  applyMoves1,
  applyMove2,
  applyMoves2,
  showTops,
  solvePart1,
  solvePart2,
}
/* Stdlib__Array Not a pure module */
