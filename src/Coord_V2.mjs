// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TableclothInt from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothInt.mjs";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.mjs";
import * as TableclothTuple2 from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothTuple2.mjs";
import * as TableclothComparator from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothComparator.mjs";

function intCompare(a, b) {
  return Stdlib__Ordering.toInt(TableclothInt.compare(a, b));
}

function compare(param) {
  return function (param$1) {
    return TableclothTuple2.compare(param, param$1, intCompare, intCompare);
  };
}

var include = TableclothComparator.Make({
      compare: compare
    });

function add(param, param$1) {
  return [
          param[0] + param$1[0] | 0,
          param[1] + param$1[1] | 0
        ];
}

function mul(param, x) {
  return [
          Math.imul(param[0], x),
          Math.imul(param[1], x)
        ];
}

var TC;

var comparator = include.comparator;

export {
  TC ,
  intCompare ,
  compare ,
  comparator ,
  add ,
  mul ,
}
/* include Not a pure module */
