// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TableclothInt from "tablecloth-rescript/src/TableclothInt.mjs";
import * as TableclothComparator from "tablecloth-rescript/src/TableclothComparator.mjs";
import * as TableclothTuple4$AdventOfCode from "./TableclothTuple4.mjs";

function compare(param) {
  return function (param$1) {
    var param$2 = TableclothInt.compare;
    var param$3 = TableclothInt.compare;
    var param$4 = TableclothInt.compare;
    var param$5 = TableclothInt.compare;
    return TableclothTuple4$AdventOfCode.compare(param, param$1, param$2, param$3, param$4, param$5);
  };
}

var include = TableclothComparator.Make({
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

var TC;

var comparator = include.comparator;

export {
  TC ,
  compare ,
  comparator ,
  add ,
  mul ,
}
/* include Not a pure module */
