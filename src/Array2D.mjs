// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "./Utils.mjs";

function make(param, e) {
  var x = param[0];
  return Stdlib__Array.makeBy(param[1], (function (param) {
                return Stdlib__Array.make(x, e);
              }));
}

function copy(t) {
  return t.map(function (__x) {
              return __x.concat([]);
            });
}

function lengthY(t) {
  return t.length;
}

function lengthX(t) {
  return Stdlib__Option.mapOr(t[0], 0, (function (a) {
                return a.length;
              }));
}

function isValidXY(t, param) {
  var y = param[1];
  var x = param[0];
  var len_x = lengthX(t);
  var len_y = t.length;
  if (x >= 0 && x <= (len_x - 1 | 0) && y >= 0) {
    return y <= (len_y - 1 | 0);
  } else {
    return false;
  }
}

function set(t, param, e) {
  var y = t[param[1]];
  if (y !== undefined) {
    y[param[0]] = e;
    return ;
  }
  
}

function setYEquals(t, y, e) {
  t[y] = e;
}

function get(t, param) {
  var y = t[param[1]];
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
    return ;
  }
  var ret = Stdlib__Array.reduce(t, [], (function (a, xs) {
          return Belt_Array.concatMany([
                      a,
                      [Stdlib__Array.getUnsafe(xs, x)]
                    ]);
        }));
  if (ret.length === t.length) {
    return ret;
  }
  
}

function map(t, f) {
  return t.map(function (x) {
              return x.map(f);
            });
}

function mapWithIndex(t, f) {
  return t.map(function (xs, j) {
              return xs.map(function (e, i) {
                          return f([
                                      i,
                                      j
                                    ], e);
                        });
            });
}

function reduce(t, a, f) {
  return Stdlib__Array.reduce(t, a, (function (acc, x) {
                return Stdlib__Array.reduce(x, acc, f);
              }));
}

function reduceWithIndex(t, a, f) {
  return Stdlib__Array.reduceWithIndex(t, a, (function (acc, xs, yi) {
                return Stdlib__Array.reduceWithIndex(xs, acc, (function (acc, x, xi) {
                              return f(acc, x, [
                                          xi,
                                          yi
                                        ]);
                            }));
              }));
}

function flatten(t) {
  var ret = [];
  for(var i = 0 ,i_finish = t.length; i < i_finish; ++i){
    ret = Belt_Array.concatMany([
          ret,
          Stdlib__Option.getWithDefault(t[i], [])
        ]);
  }
  return ret;
}

function crop(t, param, len_x, len_y) {
  var y = param[1];
  var x = param[0];
  var sizeX = lengthX(t);
  var sizeY = t.length;
  if (!isValidXY(t, [
          x,
          y
        ])) {
    return [];
  }
  var ret = [];
  var max_x_len = sizeX - x | 0;
  var adj_len_x = len_x < max_x_len ? len_x : max_x_len;
  var max_y_len = sizeY - y | 0;
  var adj_y = Caml.int_min(y + len_y | 0, y + max_y_len | 0) - 1 | 0;
  for(var i = y; i <= adj_y; ++i){
    ret = Belt_Array.concatMany([
          ret,
          [Stdlib__Option.getWithDefault(t[i], []).slice(x, x + adj_len_x | 0)]
        ]);
  }
  return ret;
}

function eq(t, u) {
  if (lengthX(t) === lengthX(u) && t.length === u.length) {
    return Belt_Array.reduceReverse2(t, u, true, (function (c, a, b) {
                  if (c) {
                    return Stdlib__Array.equal(a, b, (function (a, b) {
                                  return a === b;
                                }));
                  } else {
                    return false;
                  }
                }));
  } else {
    return false;
  }
}

function toString(t, f) {
  var arrToStr = Utils$AdventOfCode.Printable.$$Array.toString;
  return arrToStr(t.map(function (x) {
                  return x.map(f);
                }), (function (x) {
                return arrToStr(x, (function (prim) {
                              return prim;
                            })) + "\n";
              }));
}

export {
  make ,
  copy ,
  lengthX ,
  lengthY ,
  isValidXY ,
  set ,
  setYEquals ,
  get ,
  getExn ,
  getXEquals ,
  getYEquals ,
  map ,
  mapWithIndex ,
  reduce ,
  reduceWithIndex ,
  flatten ,
  crop ,
  eq ,
  toString ,
}
/* Stdlib__Array Not a pure module */
