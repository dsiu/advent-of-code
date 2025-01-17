// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Stdlib__Set from "@dsiu/rescript-stdlib-fp/src/Stdlib__Set.res.mjs";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.res.mjs";
import * as Stdlib__BigInt from "@dsiu/rescript-stdlib-fp/src/Stdlib__BigInt.res.mjs";
import * as Stdlib__Tuple2 from "@dsiu/rescript-stdlib-fp/src/Stdlib__Tuple2.res.mjs";
import * as Primitive_bigint from "rescript/lib/es6/Primitive_bigint.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.res.mjs";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.res.mjs";
import * as TableclothComparator from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothComparator.res.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function bigIntCompareOrd(a, b) {
  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
}

function bigIntCompare(a, b) {
  return Stdlib__Ordering.toInt(bigIntCompareOrd(a, b));
}

function intCompare(a, b) {
  return Stdlib__Ordering.toInt(Primitive_int.compare(a, b));
}

function compare(none, none$1) {
  return Stdlib__Tuple2.compare(none, none$1, bigIntCompare, bigIntCompare);
}

let include = TableclothComparator.Make({
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

let Coord_V2_Big_comparator = include.comparator;

let Coord_V2_Big = {
  bigIntCompareOrd: bigIntCompareOrd,
  bigIntCompare: bigIntCompare,
  intCompare: intCompare,
  compare: compare,
  comparator: Coord_V2_Big_comparator,
  add: add,
  mul: mul
};

function dumpGalaxies(galaxies) {
  galaxies._0.forEach(p => {
    console.log(p._0);
  });
}

function maxCoord(galaxies) {
  return Stdlib__Array.reduce(Stdlib__Set.toArray(galaxies._0), [
    0n,
    0n
  ], (param, param$1) => {
    let match = param$1._0;
    return [
      Primitive_bigint.max(param[0], match[0]),
      Primitive_bigint.max(match[1], param[1])
    ];
  });
}

function emptyRowCol(g) {
  let match = maxCoord(g);
  let galaxies = Stdlib__Set.toArray(g._0);
  let xs = Stdlib__Array.fromInitializer(Stdlib__BigInt.toInt(match[0]), Stdlib__Function.id);
  let cols = xs.filter(x => Stdlib__Array.count(galaxies, param => param._0[0] === BigInt(x)) === 0);
  let ys = Stdlib__Array.fromInitializer(Stdlib__BigInt.toInt(match[1]), Stdlib__Function.id);
  let rows = ys.filter(y => Stdlib__Array.count(galaxies, param => param._0[1] === BigInt(y)) === 0);
  return [
    cols,
    rows
  ];
}

function expand(galaxies, param, scale) {
  let expY = param[1];
  let expX = param[0];
  let nElemLessThan = (arr, n) => Stdlib__Array.count(arr, x => BigInt(x) < n);
  let expandXY = arr => arr.map(param => {
    let match = param._0;
    let y = match[1];
    let x = match[0];
    return {
      TAG: "Position",
      _0: [
        x + BigInt(nElemLessThan(expX, x)) * (scale - 1n),
        y + BigInt(nElemLessThan(expY, y)) * (scale - 1n)
      ]
    };
  });
  return {
    TAG: "Galaxies",
    _0: new Set(expandXY(Stdlib__Set.toArray(galaxies._0)))
  };
}

function absBigInt(x) {
  if (x < 0n) {
    return 0n - x;
  } else {
    return x;
  }
}

function manhattan(param, param$1) {
  let match = param$1._0;
  let match$1 = param._0;
  return absBigInt(match$1[0] - match[0]) + absBigInt(match$1[1] - match[1]);
}

function distances(galaxies) {
  let galaxies$1 = Stdlib__Set.toArray(galaxies._0);
  return Stdlib__Array.reduce(galaxies$1, [], (acc, p) => galaxies$1.filter(p$p => Primitive_object.notequal(p, p$p)).map(p$p => manhattan(p, p$p)).concat(acc));
}

function part1(g, scale) {
  return Primitive_bigint.div(Stdlib__Array.reduce(distances(expand(g, emptyRowCol(g), scale)), 0n, (acc, d) => acc + d), 2n);
}

function parse(data) {
  let xy = Utils.splitNewline(data).map(r => r.trim().split(""));
  return {
    TAG: "Galaxies",
    _0: new Set(Stdlib__Array.reduceWithIndex(xy, [], (acc, row, rowIndex) => Stdlib__Array.reduceWithIndex(row, [], (acc, col, colIndex) => {
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
    }).concat(acc)))
  };
}

function solvePart1(data) {
  return part1(parse(data), BigInt(2));
}

function solvePart2(data) {
  return part1(parse(data), BigInt(1000000));
}

export {
  log,
  log2,
  Coord_V2_Big,
  dumpGalaxies,
  maxCoord,
  emptyRowCol,
  expand,
  absBigInt,
  manhattan,
  distances,
  part1,
  parse,
  solvePart1,
  solvePart2,
}
/* include Not a pure module */