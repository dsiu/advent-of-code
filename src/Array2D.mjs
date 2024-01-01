// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "./Utils.mjs";

function make(param, e) {
  var x = param[0];
  return Belt_Array.makeBy(param[1], (function (param) {
                return Belt_Array.make(x, e);
              }));
}

function copy(t) {
  return Belt_Array.map(t, (function (__x) {
                return Belt_Array.concat(__x, []);
              }));
}

function lengthY(t) {
  return t.length;
}

function lengthX(t) {
  return Belt_Array.getExn(t, 0).length;
}

function isValidXY(t, param) {
  var y = param[1];
  var x = param[0];
  var len_x = Belt_Array.getExn(t, 0).length;
  var len_y = t.length;
  if (x >= 0 && x <= (len_x - 1 | 0) && y >= 0) {
    return y <= (len_y - 1 | 0);
  } else {
    return false;
  }
}

function set(t, param, e) {
  var y = Belt_Array.get(t, param[1]);
  if (y !== undefined) {
    return Belt_Array.set(y, param[0], e);
  } else {
    return false;
  }
}

var setYEquals = Belt_Array.set;

function get(t, param) {
  var y = Belt_Array.get(t, param[1]);
  if (y !== undefined) {
    return Belt_Array.get(y, param[0]);
  }
  
}

function getExn(t, param) {
  return Belt_Array.getExn(Belt_Array.getExn(t, param[1]), param[0]);
}

var getYEquals = Belt_Array.get;

function getXEquals(t, x) {
  var ret = Belt_Array.reduce(t, [], (function (a, xs) {
          return Belt_Array.concat(a, [Belt_Array.getExn(xs, x)]);
        }));
  if (ret.length === t.length) {
    return ret;
  }
  
}

function map(t, f) {
  return Belt_Array.map(t, (function (x) {
                return Belt_Array.map(x, f);
              }));
}

function mapU(t, f) {
  return Belt_Array.mapU(t, (function (x) {
                return Belt_Array.mapU(x, f);
              }));
}

function mapWithIndex(t, f) {
  return Belt_Array.mapWithIndexU(t, (function (j, xs) {
                return Belt_Array.mapWithIndexU(xs, (function (i, e) {
                              return Curry._2(f, [
                                          i,
                                          j
                                        ], e);
                            }));
              }));
}

function mapWithIndexU(t, f) {
  return Belt_Array.mapWithIndexU(t, (function (j, xs) {
                return Belt_Array.mapWithIndexU(xs, (function (i, e) {
                              return f([
                                          i,
                                          j
                                        ], e);
                            }));
              }));
}

function reduce(t, a, f) {
  return Belt_Array.reduceU(t, a, (function (acc, x) {
                return Belt_Array.reduce(x, acc, f);
              }));
}

function reduceU(t, a, f) {
  return Belt_Array.reduceU(t, a, (function (acc, x) {
                return Belt_Array.reduceU(x, acc, f);
              }));
}

function reduceWithIndex(t, a, f) {
  return Belt_Array.reduceWithIndexU(t, a, (function (acc, xs, yi) {
                return Belt_Array.reduceWithIndexU(xs, acc, (function (acc, x, xi) {
                              return Curry._3(f, acc, x, [
                                          xi,
                                          yi
                                        ]);
                            }));
              }));
}

function reduceWithIndexU(t, a, f) {
  return Belt_Array.reduceWithIndexU(t, a, (function (acc, xs, yi) {
                return Belt_Array.reduceWithIndexU(xs, acc, (function (acc, x, xi) {
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
    ret = Belt_Array.concat(ret, Belt_Option.getWithDefault(Belt_Array.get(t, i), []));
  }
  return ret;
}

function crop(t, param, len_x, len_y) {
  var y = param[1];
  var x = param[0];
  var ret = [];
  for(var i = y ,i_finish = y + len_y | 0; i < i_finish; ++i){
    ret = Belt_Array.concat(ret, [Belt_Array.slice(Belt_Option.getWithDefault(Belt_Array.get(t, i), []), x, len_x)]);
  }
  return ret;
}

function eq(t, u) {
  if (Belt_Array.getExn(t, 0).length === Belt_Array.getExn(u, 0).length && t.length === u.length) {
    return Belt_Array.reduceReverse2(t, u, true, (function (c, a, b) {
                  if (c) {
                    return Belt_Array.eq(a, b, (function (a, b) {
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
  return Curry._2(arrToStr, Belt_Array.map(t, (function (x) {
                    return Belt_Array.map(x, f);
                  })), (function (x) {
                return Curry._2(arrToStr, x, Stdlib__Function.identity) + "\n";
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
  mapU ,
  mapWithIndex ,
  mapWithIndexU ,
  reduce ,
  reduceU ,
  reduceWithIndex ,
  reduceWithIndexU ,
  flatten ,
  crop ,
  eq ,
  toString ,
}
/* No side effect */
