// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";

function powersetListMap_(set) {
  if (!set) {
    return {
            hd: /* [] */0,
            tl: /* [] */0
          };
  }
  var x = set.hd;
  var tail_powersets = powersetListMap_(set.tl);
  var with_x = Stdlib__List.map(tail_powersets, (function (it) {
          return {
                  hd: x,
                  tl: it
                };
        }));
  return Stdlib__List.concat(tail_powersets, with_x);
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
  return Stdlib__List.flatMap(tail_powersets, (function (it) {
                return {
                        hd: it,
                        tl: {
                          hd: Stdlib__List.concat({
                                hd: x,
                                tl: /* [] */0
                              }, it),
                          tl: /* [] */0
                        }
                      };
              }));
}

function powersetArrayWithList_(xs) {
  return Stdlib__List.toArray(Stdlib__List.map(powersetListMap_(Stdlib__List.fromArray(xs)), Stdlib__List.toArray));
}

function powersetArrayMap_(set) {
  var match = set.length;
  if (match === 0) {
    return [[]];
  }
  var x = Stdlib__Array.getUnsafe(set, 0);
  var xs = set.slice(1);
  var tail_powersets = powersetArrayMap_(xs);
  var with_x = tail_powersets.map(function (it) {
        return [x].concat(it);
      });
  return tail_powersets.concat(with_x);
}

function flatMap(prim0, prim1) {
  return prim0.flatMap(prim1);
}

function powersetArrayFlatMap_(set) {
  var match = set.length;
  if (match === 0) {
    return [[]];
  }
  var x = Stdlib__Array.getUnsafe(set, 0);
  var xs = set.slice(1);
  var tail_powersets = powersetArrayFlatMap_(xs);
  return tail_powersets.flatMap(function (it) {
              return [
                      it,
                      [x].concat(it)
                    ];
            });
}

var List;

var powersetList = powersetListFlatMap_;

var $$Array;

var powersetArray = powersetArrayFlatMap_;

export {
  List ,
  powersetListMap_ ,
  powersetListFlatMap_ ,
  powersetList ,
  powersetArrayWithList_ ,
  $$Array ,
  powersetArrayMap_ ,
  flatMap ,
  powersetArrayFlatMap_ ,
  powersetArray ,
}
/* Stdlib__List Not a pure module */
