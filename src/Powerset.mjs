// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as FP_Utils$AdventOfCode from "./FP_Utils.mjs";

function powersetListMap_(set) {
  if (!set) {
    return {
            hd: /* [] */0,
            tl: /* [] */0
          };
  }
  var x = set.hd;
  var tail_powersets = powersetListMap_(set.tl);
  var with_x = Belt_List.map(tail_powersets, (function (it) {
          return {
                  hd: x,
                  tl: it
                };
        }));
  return Belt_List.concat(tail_powersets, with_x);
}

function powersetListFlatMap_(set) {
  if (!set) {
    return {
            hd: /* [] */0,
            tl: /* [] */0
          };
  }
  var x = set.hd;
  var tail_powersets = powersetListFlatMap_(set.tl);
  return FP_Utils$AdventOfCode.flatMapList(tail_powersets, (function (it) {
                return {
                        hd: it,
                        tl: {
                          hd: Belt_List.concat({
                                hd: x,
                                tl: /* [] */0
                              }, it),
                          tl: /* [] */0
                        }
                      };
              }));
}

function powersetArrayWithList_(xs) {
  return Belt_List.toArray(Belt_List.map(powersetListMap_(Belt_List.fromArray(xs)), Belt_List.toArray));
}

function powersetArrayMap_(set) {
  var match = set.length;
  if (match === 0) {
    return [[]];
  }
  var x = Belt_Array.getExn(set, 0);
  var xs = Belt_Array.sliceToEnd(set, 1);
  var tail_powersets = powersetArrayMap_(xs);
  var with_x = Belt_Array.map(tail_powersets, (function (it) {
          return Belt_Array.concat([x], it);
        }));
  return Belt_Array.concat(tail_powersets, with_x);
}

function powersetArrayFlatMap_(set) {
  var match = set.length;
  if (match === 0) {
    return [[]];
  }
  var x = Belt_Array.getExn(set, 0);
  var xs = Belt_Array.sliceToEnd(set, 1);
  var tail_powersets = powersetArrayFlatMap_(xs);
  return FP_Utils$AdventOfCode.flatMapArray(tail_powersets, (function (it) {
                return [
                        it,
                        Belt_Array.concat([x], it)
                      ];
              }));
}

var List;

var flatMapList = FP_Utils$AdventOfCode.flatMapList;

var powersetList = powersetListFlatMap_;

var $$Array;

var flatMapArray = FP_Utils$AdventOfCode.flatMapArray;

var powersetArray = powersetArrayFlatMap_;

export {
  List ,
  powersetListMap_ ,
  flatMapList ,
  powersetListFlatMap_ ,
  powersetList ,
  powersetArrayWithList_ ,
  $$Array ,
  powersetArrayMap_ ,
  flatMapArray ,
  powersetArrayFlatMap_ ,
  powersetArray ,
}
/* No side effect */
