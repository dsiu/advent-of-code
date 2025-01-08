// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.mjs";
import * as Array2D from "../../Array2D.mjs";
import * as Belt_Int from "rescript/lib/es6/Belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function addMark(m1, m2) {
  if (m1 === "#" || m2 === "#") {
    return "#";
  } else {
    return ".";
  }
}

function countMark(p) {
  return Array2D.reduce(p, 0, (a, mark) => {
    if (mark === "#") {
      return a + 1 | 0;
    } else {
      return a;
    }
  });
}

function makeFoldDirection(s) {
  switch (s) {
    case "x" :
      return "X";
    case "y" :
      return "Y";
    default:
      throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
  }
}

function getPaper(t) {
  return t.paper;
}

function getFolds(t) {
  return t.folds;
}

function findSize(t) {
  let param = Belt_Array.reduce(t, [
    0,
    0
  ], (param, param$1) => {
    let y = param$1[1];
    let x = param$1[0];
    let ya = param[1];
    let xa = param[0];
    return [
      x > xa ? x : xa,
      y > ya ? y : ya
    ];
  });
  return [
    param[0] + 1 | 0,
    param[1] + 1 | 0
  ];
}

function makePaper(coords) {
  let sizes = findSize(coords);
  let p = Array2D.make(sizes, ".");
  Belt_Array.forEach(coords, c => Array2D.set(p, c, "#"));
  return p;
}

function makeFolds(folds) {
  return Belt_Array.map(folds, param => [
    makeFoldDirection(param[0]),
    param[1]
  ]);
}

function make(coords, folds) {
  return {
    paper: makePaper(coords),
    folds: makeFolds(folds)
  };
}

function transformCoord(param, param$1) {
  let n = param$1[1];
  let y = param[1];
  let x = param[0];
  if (param$1[0] === "Y") {
    if (y >= n) {
      return [
        x,
        (n << 1) - y | 0
      ];
    } else {
      return [
        x,
        y
      ];
    }
  } else if (x >= n) {
    return [
      (n << 1) - x | 0,
      y
    ];
  } else {
    return [
      x,
      y
    ];
  }
}

function getTransformedSize(t, param) {
  let n = param[1];
  let sizeX = Array2D.lengthX(t);
  let sizeY = Array2D.lengthY(t);
  if (param[0] === "Y") {
    return [
      sizeX,
      n
    ];
  } else {
    return [
      n,
      sizeY
    ];
  }
}

function transform(t, f) {
  let new_size = getTransformedSize(t, f);
  let t$p = Array2D.make(new_size, ".");
  return Array2D.reduceWithIndex(t, t$p, (a, mark, coord) => {
    let coord$p = transformCoord(coord, f);
    let new_mark = Array2D.get(t, coord$p);
    if (new_mark !== undefined) {
      Array2D.set(a, coord$p, addMark(mark, new_mark));
    }
    return a;
  });
}

function transformFirst(t) {
  return transform(t.paper, Belt_Option.getExn(Belt_Array.get(t.folds, 0)));
}

function transformAll(t) {
  let folds = t.folds;
  return Belt_Array.reduce(folds, t.paper, transform);
}

function toString(t) {
  return Belt_Array.map(t, x => x.join("")).join("\n");
}

let Paper = {
  addMark: addMark,
  countMark: countMark,
  makeFoldDirection: makeFoldDirection,
  getPaper: getPaper,
  getFolds: getFolds,
  findSize: findSize,
  makePaper: makePaper,
  makeFolds: makeFolds,
  make: make,
  transformCoord: transformCoord,
  getTransformedSize: getTransformedSize,
  transform: transform,
  transformFirst: transformFirst,
  transformAll: transformAll,
  toString: toString
};

function parse(data) {
  let parsed = Utils.splitDoubleNewline(data);
  let coords = Belt_Option.getExn(Belt_Array.get(parsed, 0));
  let folds = Belt_Option.getExn(Belt_Array.get(parsed, 1));
  return [
    Belt_Array.map(Utils.splitNewline(coords), x => {
      let s = x.trim().split(",");
      return [
        Belt_Option.getExn(Belt_Option.flatMap(Belt_Array.get(s, 0), Belt_Int.fromString)),
        Belt_Option.getExn(Belt_Option.flatMap(Belt_Array.get(s, 1), Belt_Int.fromString))
      ];
    }),
    Belt_Array.map(Utils.splitNewline(folds), x => {
      let i = "fold along ".length;
      let sub = x.trim().slice(i, x.length);
      let s = sub.split("=");
      return [
        Belt_Option.getExn(Belt_Array.get(s, 0)),
        Belt_Option.getExn(Belt_Option.flatMap(Belt_Array.get(s, 1), Belt_Int.fromString))
      ];
    })
  ];
}

function solvePart1(data) {
  let match = parse(data);
  let p = make(match[0], match[1]);
  return countMark(transformFirst(p));
}

function solvePart2(data) {
  let match = parse(data);
  let p = make(match[0], match[1]);
  return countMark(transformAll(p));
}

export {
  log,
  log2,
  Paper,
  parse,
  solvePart1,
  solvePart2,
}
/* Utils Not a pure module */
