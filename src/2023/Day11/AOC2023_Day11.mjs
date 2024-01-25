// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__BigInt from "@dsiu/rescript-stdlib-fp/src/Stdlib__BigInt.mjs";
import * as Stdlib__Tuple2 from "@dsiu/rescript-stdlib-fp/src/Stdlib__Tuple2.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.mjs";
import * as Stdlib__BigInt_Ext from "@dsiu/rescript-stdlib-fp/src/Stdlib__BigInt_Ext.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as TableclothComparator from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothComparator.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function bigIntCompareOrd(a, b) {
  if (Caml_obj.lessthan(a, b)) {
    return -1;
  } else if (Caml_obj.greaterthan(a, b)) {
    return 1;
  } else {
    return 0;
  }
}

function bigIntCompare(a, b) {
  return Stdlib__Ordering.toInt(bigIntCompareOrd(a, b));
}

function intCompare(a, b) {
  return Stdlib__Ordering.toInt(Stdlib__Int.compare(a, b));
}

function compare(param) {
  return function (param$1) {
    return Stdlib__Tuple2.compare(param, param$1, bigIntCompare, bigIntCompare);
  };
}

var include = TableclothComparator.Make({
      compare: compare
    });

function add(param, param$1) {
  return [
          param[0] + param$1[0],
          param[1] + param$1[1]
        ];
}

function mul(param, x) {
  return [
          param[0] * x,
          param[1] * x
        ];
}

var Coord_V2_Big_comparator = include.comparator;

var Coord_V2_Big = {
  bigIntCompareOrd: bigIntCompareOrd,
  bigIntCompare: bigIntCompare,
  intCompare: intCompare,
  compare: compare,
  comparator: Coord_V2_Big_comparator,
  add: add,
  mul: mul
};

function dumpGalaxies(galaxies) {
  galaxies._0.forEach(function (p) {
        console.log(p._0);
      });
}

function setToArray(set) {
  return Array.from(set.values());
}

function maxBigInt(m, n) {
  if (Caml_obj.lessthan(m, n)) {
    return m;
  } else {
    return n;
  }
}

function maxCoord(galaxies) {
  var origin_0 = Stdlib__BigInt_Ext.Constants.zero;
  var origin_1 = Stdlib__BigInt_Ext.Constants.zero;
  var origin = [
    origin_0,
    origin_1
  ];
  return Stdlib__Array.reduce(Array.from(galaxies._0.values()), origin, (function (param, param$1) {
                var match = param$1._0;
                return [
                        Caml_obj.max(param[0], match[0]),
                        Caml_obj.max(match[1], param[1])
                      ];
              }));
}

function emptyRowCol(g) {
  var match = maxCoord(g);
  var galaxies = Array.from(g._0.values());
  var xs = Stdlib__Array.fromInitializer(Stdlib__BigInt.toInt(match[0]), Stdlib__Function.id);
  var cols = xs.filter(function (x) {
        return galaxies.filter(function (param) {
                    return Caml_obj.equal(param._0[0], BigInt(x));
                  }).length === 0;
      });
  var ys = Stdlib__Array.fromInitializer(Stdlib__BigInt.toInt(match[1]), Stdlib__Function.id);
  var rows = ys.filter(function (y) {
        return galaxies.filter(function (param) {
                    return Caml_obj.equal(param._0[1], BigInt(y));
                  }).length === 0;
      });
  return [
          cols,
          rows
        ];
}

function expand(galaxies, param, scale) {
  var expY = param[1];
  var expX = param[0];
  var nElemLessThan = function (arr, n) {
    return Stdlib__Array.count(arr, (function (x) {
                  return Caml_obj.lessthan(BigInt(x), n);
                }));
  };
  var expandXY = function (arr) {
    return arr.map(function (param) {
                var match = param._0;
                var y = match[1];
                var x = match[0];
                return {
                        TAG: "Position",
                        _0: [
                          x + BigInt(nElemLessThan(expX, x)) * (scale - Stdlib__BigInt_Ext.Constants.one),
                          y + BigInt(nElemLessThan(expY, y)) * (scale - Stdlib__BigInt_Ext.Constants.one)
                        ]
                      };
              });
  };
  return {
          TAG: "Galaxies",
          _0: new Set(expandXY(Array.from(galaxies._0.values())))
        };
}

function absBigInt(x) {
  if (Caml_obj.lessthan(x, Stdlib__BigInt_Ext.Constants.zero)) {
    return Stdlib__BigInt_Ext.Constants.zero - x;
  } else {
    return x;
  }
}

function manhattan(param, param$1) {
  var match = param$1._0;
  var match$1 = param._0;
  return absBigInt(match$1[0] - match[0]) + absBigInt(match$1[1] - match[1]);
}

function distances(galaxies) {
  var galaxies$1 = Array.from(galaxies._0.values());
  return Stdlib__Array.reduce(galaxies$1, [], (function (acc, p) {
                return galaxies$1.filter(function (p$p) {
                                return Caml_obj.notequal(p, p$p);
                              }).map(function (p$p) {
                              return manhattan(p, p$p);
                            }).concat(acc);
              }));
}

function part1(g, scale) {
  return Stdlib__Array.reduce(distances(expand(g, emptyRowCol(g), scale)), Stdlib__BigInt_Ext.Constants.zero, (function (acc, d) {
                return acc + d;
              })) / BigInt(2);
}

function parse(data) {
  var xy = Utils$AdventOfCode.splitNewline(data).map(function (r) {
        return r.trim().split("");
      });
  return {
          TAG: "Galaxies",
          _0: new Set(Stdlib__Array.reduceWithIndex(xy, [], (function (acc, row, rowIndex) {
                      return Stdlib__Array.reduceWithIndex(row, [], (function (acc, col, colIndex) {
                                      if (col === "#") {
                                        acc.push({
                                              TAG: "Position",
                                              _0: [
                                                BigInt(colIndex),
                                                BigInt(rowIndex)
                                              ]
                                            });
                                        return acc;
                                      } else {
                                        return acc;
                                      }
                                    })).concat(acc);
                    })))
        };
}

function solvePart1(data) {
  return part1(parse(data), BigInt(2));
}

function solvePart2(data) {
  return part1(parse(data), BigInt(1000000));
}

export {
  log ,
  log2 ,
  Coord_V2_Big ,
  dumpGalaxies ,
  setToArray ,
  maxBigInt ,
  maxCoord ,
  emptyRowCol ,
  expand ,
  absBigInt ,
  manhattan ,
  distances ,
  part1 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* include Not a pure module */
