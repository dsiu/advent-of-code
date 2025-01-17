// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.res.mjs";
import * as TableclothTuple4 from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothTuple4.res.mjs";
import * as TableclothComparator from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothComparator.res.mjs";

function intCompare(a, b) {
  return Stdlib__Ordering.toInt(Primitive_int.compare(a, b));
}

function compare(a, b) {
  return TableclothTuple4.compare(a, b, intCompare, intCompare, intCompare, intCompare);
}

let include = TableclothComparator.Make({
  compare: compare
});

function add(param, param$1) {
  return [
    param[0] + param$1[0] | 0,
    param[1] + param$1[1] | 0,
    param[2] + param$1[2] | 0,
    param[3] + param$1[3] | 0
  ];
}

function mul(param, x) {
  return [
    Math.imul(param[0], x),
    Math.imul(param[1], x),
    Math.imul(param[2], x),
    Math.imul(param[3], x)
  ];
}

let TC;

let comparator = include.comparator;

export {
  TC,
  intCompare,
  compare,
  comparator,
  add,
  mul,
}
/* include Not a pure module */