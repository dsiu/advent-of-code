// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_string from "rescript/lib/es6/caml_string.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Belt_SetString from "rescript/lib/es6/belt_SetString.js";
import * as TableclothChar from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothChar.mjs";
import * as TableclothArray from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothArray.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function parse(data, fn) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (s) {
                return Curry._1(fn, Utils$AdventOfCode.splitChars(s.trim()));
              }));
}

function stringToChar(__x) {
  return Caml_string.get(__x, 0);
}

function charToPriority(item) {
  var c = Caml_string.get(item, 0);
  if (TableclothChar.isUppercase(c)) {
    return ((TableclothChar.toCode(c) - /* 'A' */65 | 0) + 1 | 0) + 26 | 0;
  } else {
    return (TableclothChar.toCode(c) - /* 'a' */97 | 0) + 1 | 0;
  }
}

function commonItem(param) {
  return Belt_Array.getExn(Belt_SetString.toArray(Belt_SetString.intersect(Belt_SetString.fromArray(param._0), Belt_SetString.fromArray(param._1))), 0);
}

function part1(rucksacks) {
  return Utils$AdventOfCode.sumIntArray(Belt_Array.map(rucksacks, (function (param) {
                    return Utils$AdventOfCode.compose(commonItem, charToPriority, param);
                  })));
}

function merge(param) {
  return Belt_SetString.union(Belt_SetString.fromArray(param._0), Belt_SetString.fromArray(param._1));
}

function badgeOf(rucksacks) {
  return Belt_Array.getExn(Belt_SetString.toArray(Stdlib__Array.foldl1(Belt_Array.map(rucksacks, merge), Belt_SetString.intersect)), 0);
}

function part2(rucksacks) {
  var groups = TableclothArray.chunksOf(rucksacks, 3);
  var badges = TableclothArray.map(groups, badgeOf);
  return Utils$AdventOfCode.sumIntArray(Belt_Array.map(badges, charToPriority));
}

function mkRucksack(xs) {
  var mid = (xs.length >> 1);
  return {
          TAG: "Rucksack",
          _0: Belt_Array.slice(xs, 0, mid),
          _1: Belt_Array.slice(xs, mid, mid)
        };
}

function solvePart1(data) {
  return part1(parse(data, mkRucksack));
}

function solvePart2(data) {
  return part2(parse(data, mkRucksack));
}

var TC;

export {
  TC ,
  log ,
  parse ,
  stringToChar ,
  charToPriority ,
  commonItem ,
  part1 ,
  merge ,
  badgeOf ,
  part2 ,
  mkRucksack ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Array Not a pure module */
