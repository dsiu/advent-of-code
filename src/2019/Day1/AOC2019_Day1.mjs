// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as TableclothInt from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothInt.mjs";
import * as TableclothArray from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothArray.mjs";

function log(prim) {
  console.log(prim);
}

function fuel(mass) {
  return (mass / 3 | 0) - 2 | 0;
}

function part1(xs) {
  return TableclothArray.sum(Belt_Array.map(xs, fuel), {
    zero: TableclothInt.zero,
    add: TableclothInt.add
  });
}

function fuelCompound(mass) {
  let _mass = mass;
  let _acc = 0;
  while (true) {
    let acc = _acc;
    let mass$1 = _mass;
    let mass$p = fuel(mass$1);
    if (mass$p <= 0) {
      return acc;
    }
    _acc = acc + mass$p | 0;
    _mass = mass$p;
    continue;
  };
}

function part2(xs) {
  return TableclothArray.sum(Belt_Array.map(xs, fuelCompound), {
    zero: TableclothInt.zero,
    add: TableclothInt.add
  });
}

function parse(data) {
  return Belt_Array.map(Utils.splitNewline(data), Utils.intFromStringExn);
}

function solvePart1(data) {
  return part1(Belt_Array.map(Utils.splitNewline(data), Utils.intFromStringExn));
}

function solvePart2(data) {
  return part2(Belt_Array.map(Utils.splitNewline(data), Utils.intFromStringExn));
}

let TC;

export {
  log,
  TC,
  fuel,
  part1,
  fuelCompound,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */
