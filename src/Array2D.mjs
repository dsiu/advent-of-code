// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "./Utils.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";

function make(param, e) {
  let x = param[0];
  return Stdlib__Array.makeBy(param[1], param => Stdlib__Array.make(x, e));
}

function copy(t) {
  return t.map(__x => __x.concat([]));
}

function lengthY(t) {
  return t.length;
}

function lengthX(t) {
  return Stdlib__Option.mapOr(t[0], 0, a => a.length);
}

function isValidXY(t, param) {
  let y = param[1];
  let x = param[0];
  let len_x = lengthX(t);
  let len_y = t.length;
  if (x >= 0 && x <= (len_x - 1 | 0) && y >= 0) {
    return y <= (len_y - 1 | 0);
  } else {
    return false;
  }
}

function set(t, param, e) {
  let y = t[param[1]];
  if (y !== undefined) {
    y[param[0]] = e;
    return;
  }
  
}

function setYEquals(t, y, e) {
  t[y] = e;
}

function get(t, param) {
  let y = t[param[1]];
  if (y !== undefined) {
    return y[param[0]];
  }
  
}

function getExn(t, param) {
  return Stdlib__Array.getUnsafe(Stdlib__Array.getUnsafe(t, param[1]), param[0]);
}

function getYEquals(t, y) {
  return t[y];
}

function getXEquals(t, x) {
  if (x >= lengthX(t)) {
    return;
  }
  let ret = Stdlib__Array.reduce(t, [], (a, xs) => Belt_Array.concatMany([
    a,
    [Stdlib__Array.getUnsafe(xs, x)]
  ]));
  if (ret.length === t.length) {
    return ret;
  }
  
}

function map(t, f) {
  return t.map(x => x.map(f));
}

function mapWithIndex(t, f) {
  return t.map((xs, j) => xs.map((e, i) => f([
    i,
    j
  ], e)));
}

function reduce(t, a, f) {
  return Stdlib__Array.reduce(t, a, (acc, x) => Stdlib__Array.reduce(x, acc, f));
}

function reduceWithIndex(t, a, f) {
  return Stdlib__Array.reduceWithIndex(t, a, (acc, xs, yi) => Stdlib__Array.reduceWithIndex(xs, acc, (acc, x, xi) => f(acc, x, [
    xi,
    yi
  ])));
}

function flatten(t) {
  let ret = [];
  for (let i = 0, i_finish = t.length; i < i_finish; ++i) {
    ret = Belt_Array.concatMany([
      ret,
      Stdlib__Option.getOr(t[i], [])
    ]);
  }
  return ret;
}

function crop(t, param, len_x, len_y) {
  let y = param[1];
  let x = param[0];
  let sizeX = lengthX(t);
  let sizeY = t.length;
  if (!isValidXY(t, [
      x,
      y
    ])) {
    return [];
  }
  let ret = [];
  let max_x_len = sizeX - x | 0;
  let adj_len_x = Primitive_int.min(len_x, max_x_len);
  let max_y_len = sizeY - y | 0;
  let adj_y = Primitive_int.min(y + len_y | 0, y + max_y_len | 0) - 1 | 0;
  for (let i = y; i <= adj_y; ++i) {
    ret = Belt_Array.concatMany([
      ret,
      [Stdlib__Option.getOr(t[i], []).slice(x, x + adj_len_x | 0)]
    ]);
  }
  return ret;
}

function eq(t, u) {
  if (lengthX(t) === lengthX(u) && t.length === u.length) {
    return Belt_Array.reduceReverse2(t, u, true, (c, a, b) => {
      if (c) {
        return Stdlib__Array.equal(a, b, (a, b) => a === b);
      } else {
        return false;
      }
    });
  } else {
    return false;
  }
}

function toString(t, f) {
  let arrToStr = Utils.Printable.$$Array.toString;
  return arrToStr(t.map(x => x.map(f)), x => arrToStr(x, prim => prim) + "\n");
}

export {
  make,
  copy,
  lengthX,
  lengthY,
  isValidXY,
  set,
  setYEquals,
  get,
  getExn,
  getXEquals,
  getYEquals,
  map,
  mapWithIndex,
  reduce,
  reduceWithIndex,
  flatten,
  crop,
  eq,
  toString,
}
/* Utils Not a pure module */
