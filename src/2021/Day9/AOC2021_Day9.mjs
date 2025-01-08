// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.mjs";
import * as Array2D from "../../Array2D.mjs";
import * as Belt_Id from "rescript/lib/es6/Belt_Id.js";
import * as Belt_Set from "rescript/lib/es6/Belt_Set.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Coordinate from "../../Coordinate.mjs";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Belt_SortArrayInt from "rescript/lib/es6/Belt_SortArrayInt.js";

function log(prim) {
  console.log(prim);
}

function adjCoords(c) {
  return Belt_List.map({
    hd: Coordinate.StepFunctions.stepN,
    tl: {
      hd: Coordinate.StepFunctions.stepW,
      tl: {
        hd: Coordinate.StepFunctions.stepE,
        tl: {
          hd: Coordinate.StepFunctions.stepS,
          tl: /* [] */0
        }
      }
    }
  }, f => f(c));
}

function getAdjacents(t, param) {
  return Belt_List.keepMap(adjCoords([
    param[0],
    param[1]
  ]), c => {
    if (Array2D.isValidXY(t, c)) {
      return {
        TAG: "CoordAndVal",
        _0: c,
        _1: Array2D.getExn(t, c)
      };
    }
    
  });
}

function isLowest(x, _adjs) {
  while (true) {
    let adjs = _adjs;
    if (!adjs) {
      return true;
    }
    if (x >= adjs.hd._1) {
      return false;
    }
    _adjs = adjs.tl;
    continue;
  };
}

function getLowPoints(t) {
  return Array2D.reduceWithIndex(t, [], (a, p, param) => {
    let y = param[1];
    let x = param[0];
    if (isLowest(p, getAdjacents(t, [
        x,
        y
      ]))) {
      return Belt_Array.concat(a, [{
          TAG: "CoordAndVal",
          _0: [
            x,
            y
          ],
          _1: p
        }]);
    } else {
      return a;
    }
  });
}

function getAllPoints(t) {
  return Array2D.reduceWithIndex(t, [], (a, p, param) => Belt_Array.concat(a, [{
      TAG: "CoordAndVal",
      _0: [
        param[0],
        param[1]
      ],
      _1: p
    }]));
}

function cmp(param, param$1) {
  let c = Primitive_object.compare(param[0], param$1[0]);
  if (c !== 0) {
    return c;
  } else {
    return Primitive_object.compare(param[1], param$1[1]);
  }
}

let PairComparator = Belt_Id.MakeComparable({
  cmp: cmp
});

function getBasin(t, param) {
  let helper = (t, param, visited) => {
    let y = param[1];
    let x = param[0];
    let v = Array2D.getExn(t, [
      x,
      y
    ]);
    if (v < 0) {
      return visited;
    }
    let new_set = Belt_Set.add(visited, [
      x,
      y
    ]);
    Array2D.set(t, [
      x,
      y
    ], -1);
    let candidates = Belt_List.keep(getAdjacents(t, [
      x,
      y
    ]), param => {
      if (param._1 !== 9 && v >= 0) {
        return !Belt_Set.has(new_set, param._0);
      } else {
        return false;
      }
    });
    return Belt_List.reduce(candidates, new_set, (acc, param) => {
      let match = param._0;
      return Belt_Set.union(acc, helper(t, [
        match[0],
        match[1]
      ], acc));
    });
  };
  let b_set = Belt_Set.make(PairComparator);
  return helper(t, [
    param[0],
    param[1]
  ], b_set);
}

function getBasinSize(t, param) {
  return Belt_Set.size(getBasin(t, [
    param[0],
    param[1]
  ]));
}

function search(grid, x, y) {
  let row = Belt_Option.getWithDefault(Belt_Array.get(grid, y), []);
  let height = Belt_Option.getWithDefault(Belt_Array.get(row, x), 10);
  if (height >= 9 || height < 0) {
    return 0;
  } else {
    Belt_Array.set(row, x, -1);
    return (((1 + search(grid, x - 1 | 0, y) | 0) + search(grid, x + 1 | 0, y) | 0) + search(grid, x, y - 1 | 0) | 0) + search(grid, x, y + 1 | 0) | 0;
  }
}

function make(xs) {
  let x = Belt_Array.getExn(xs, 0).length;
  let y = xs.length;
  let ret = Array2D.make([
    x,
    y
  ], 0);
  Belt_Array.forEachWithIndex(xs, (y, ys) => Belt_Array.forEachWithIndex(ys, (x, c) => {
    Array2D.set(ret, [
      x,
      y
    ], c);
  }));
  return ret;
}

let HeightMap = {
  adjCoords: adjCoords,
  getAdjacents: getAdjacents,
  isLowest: isLowest,
  getLowPoints: getLowPoints,
  getAllPoints: getAllPoints,
  PairComparator: PairComparator,
  getBasin: getBasin,
  getBasinSize: getBasinSize,
  search: search,
  make: make
};

function parse(data) {
  return Belt_Array.map(Utils.splitNewline(data), extra => Stdlib__Function.compose(prim => prim.trim(), x => Belt_Array.map(Utils.splitChars(x), Utils.intFromStringExn), extra));
}

function solvePart1(data) {
  let hmap = make(parse(data));
  return Belt_Array.reduce(Belt_Array.map(getLowPoints(hmap), param => Utils.add(param._1, 1)), 0, Utils.add);
}

function solvePart2(data) {
  let hmap = make(parse(data));
  let basins = Belt_Array.reverse(Belt_SortArrayInt.stableSort(Belt_Array.map(getLowPoints(hmap), param => getBasinSize(hmap, param._0))));
  let largest3 = Belt_Array.slice(basins, 0, 3);
  return Belt_Array.reduce(largest3, 1, Utils.mul);
}

function solvePart2_from_github(data) {
  let hmap = make(parse(data));
  let basins = Belt_Array.reverse(Belt_SortArrayInt.stableSort(Belt_Array.map(getLowPoints(hmap), param => {
    let match = param._0;
    return search(hmap, match[0], match[1]);
  })));
  let largest3 = Belt_Array.slice(basins, 0, 3);
  return Belt_Array.reduce(largest3, 1, Utils.mul);
}

export {
  log,
  HeightMap,
  parse,
  solvePart1,
  solvePart2,
  solvePart2_from_github,
}
/* PairComparator Not a pure module */
