// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";

function log(prim) {
  console.log(prim);
}

function width(t) {
  return Belt_Option.getExn(Belt_Array.get(t, 0)).length;
}

function height(t) {
  return t.length;
}

function get(t, param) {
  return Array2D$AdventOfCode.get(t, [
    param[0],
    param[1]
  ]);
}

function getWrapped(t, param) {
  let w = width(t);
  let h = t.length;
  let newX = Primitive_int.mod_(param[0], w);
  let newY = Primitive_int.mod_(param[1], h);
  return Array2D$AdventOfCode.get(t, [
    newX,
    newY
  ]);
}

function isTree(c) {
  return c === "#";
}

function make(xs) {
  let x = Belt_Array.getExn(xs, 0).length;
  let y = xs.length;
  let ret = Array2D$AdventOfCode.make([
    x,
    y
  ], "");
  Belt_Array.forEachWithIndex(xs, (j, s) => {
    let ss = s.split("");
    Belt_Array.forEachWithIndex(ss, (i, c) => {
      Array2D$AdventOfCode.set(ret, [
        i,
        j
      ], c);
    });
  });
  return ret;
}

function walk(t, param) {
  let down = param[1];
  let side = param[0];
  let h = t.length;
  let _count = 0;
  let _param = [
    0,
    0
  ];
  while (true) {
    let param$1 = _param;
    let count = _count;
    let y = param$1[1];
    let x = param$1[0];
    let nextX = x + side | 0;
    let nextY = y + down | 0;
    let cur = Belt_Option.getExn(getWrapped(t, [
      x,
      y
    ])) === "#";
    let done = y >= h;
    if (done) {
      return count;
    }
    if (cur) {
      _param = [
        nextX,
        nextY
      ];
      _count = count + 1 | 0;
      continue;
    }
    _param = [
      nextX,
      nextY
    ];
    continue;
  };
}

let TreeMap = {
  width: width,
  height: height,
  get: get,
  getWrapped: getWrapped,
  isTree: isTree,
  make: make,
  walk: walk
};

function solvePart1(data) {
  let parsed = Belt_Array.map(data.split("\n"), prim => prim.trim());
  return walk(make(parsed), [
    3,
    1
  ]);
}

function solvePart2(data) {
  let parsed = Belt_Array.map(data.split("\n"), prim => prim.trim());
  let tm = make(parsed);
  let slopes = [
    [
      1,
      1
    ],
    [
      3,
      1
    ],
    [
      5,
      1
    ],
    [
      7,
      1
    ],
    [
      1,
      2
    ]
  ];
  let trees = Belt_Array.map(slopes, s => walk(tm, s));
  let acc = BigInt(1);
  return Belt_Array.reduce(trees, acc, (a, s) => {
    let ss = BigInt(s);
    return a * ss;
  }).toString();
}

export {
  log,
  TreeMap,
  solvePart1,
  solvePart2,
}
/* Array2D-AdventOfCode Not a pure module */
