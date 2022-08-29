// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function flatMapList(xs, f) {
  return Belt_List.reduce(Belt_List.map(xs, f), /* [] */0, Belt_List.concat);
}

function listToOption(l) {
  if (l) {
    return Caml_option.some(l.hd);
  }
  
}

function combinationList2(a, b, f) {
  return Belt_List.reduce(a, /* [] */0, (function (acc, x) {
                return Belt_List.concat(acc, Belt_List.reduce(b, /* [] */0, (function (acc, y) {
                                  return Belt_List.concat(acc, {
                                              hd: Curry._2(f, x, y),
                                              tl: /* [] */0
                                            });
                                })));
              }));
}

function combinationIfList2(a, b, f) {
  return Belt_List.reduce(a, /* [] */0, (function (acc, x) {
                return Belt_List.concat(acc, Belt_List.reduce(b, /* [] */0, (function (acc, y) {
                                  var r = Curry._2(f, x, y);
                                  if (r !== undefined) {
                                    return Belt_List.concat(acc, {
                                                hd: Caml_option.valFromOption(r),
                                                tl: /* [] */0
                                              });
                                  } else {
                                    return acc;
                                  }
                                })));
              }));
}

function flatMapArray(xs, f) {
  return Belt_Array.reduce(Belt_Array.map(xs, f), [], Belt_Array.concat);
}

function arrayToOption(__x) {
  return Belt_Array.get(__x, 0);
}

function foldLeftArray(xs, f) {
  var init = Belt_Array.getExn(xs, 0);
  var rest = Belt_Array.sliceToEnd(xs, 1);
  return Belt_Array.reduce(rest, init, f);
}

function foldRightArray(xs, f) {
  var end = xs.length - 1 | 0;
  var init = Belt_Array.getExn(xs, end);
  var rest = Belt_Array.slice(xs, 0, end);
  return Belt_Array.reduceReverse(rest, init, f);
}

function combinationArray2(a, b, f) {
  return Belt_Array.reduce(a, [], (function (acc, x) {
                return Belt_Array.concat(acc, Belt_Array.reduce(b, [], (function (acc, y) {
                                  return Belt_Array.concat(acc, [Curry._2(f, x, y)]);
                                })));
              }));
}

function combinationArray3(a, b, c, f) {
  return Belt_Array.reduce(a, [], (function (acc, x) {
                return Belt_Array.concat(acc, Belt_Array.reduce(b, [], (function (acc, y) {
                                  return Belt_Array.concat(acc, Belt_Array.reduce(c, [], (function (acc, z) {
                                                    return Belt_Array.concat(acc, [Curry._3(f, x, y, z)]);
                                                  })));
                                })));
              }));
}

function combinationIfArray2(a, b, f) {
  return Belt_Array.reduce(a, [], (function (acc, x) {
                return Belt_Array.concat(acc, Belt_Array.reduce(b, [], (function (acc, y) {
                                  var r = Curry._2(f, x, y);
                                  if (r !== undefined) {
                                    return Belt_Array.concat(acc, [Caml_option.valFromOption(r)]);
                                  } else {
                                    return acc;
                                  }
                                })));
              }));
}

function combinationIfArray3(a, b, c, f) {
  return Belt_Array.reduce(a, [], (function (acc, x) {
                return Belt_Array.concat(acc, Belt_Array.reduce(b, [], (function (acc, y) {
                                  return Belt_Array.concat(acc, Belt_Array.reduce(c, [], (function (acc, z) {
                                                    var r = Curry._3(f, x, y, z);
                                                    if (r !== undefined) {
                                                      return Belt_Array.concat(acc, [Caml_option.valFromOption(r)]);
                                                    } else {
                                                      return acc;
                                                    }
                                                  })));
                                })));
              }));
}

function optionOr(a, b) {
  if (a !== undefined) {
    return a;
  } else {
    return b;
  }
}

function identity(a) {
  return a;
}

function eq(x, y) {
  return x === y;
}

function composeU(f, g, x) {
  return g(f(x));
}

function compose(f, g, x) {
  return Curry._1(g, Curry._1(f, x));
}

function compose3(f, g, h, x) {
  return Curry._1(h, Curry._1(g, Curry._1(f, x)));
}

function compose4(f, g, h, i, x) {
  return Curry._1(i, Curry._1(h, Curry._1(g, Curry._1(f, x))));
}

function composeN(fs) {
  return foldLeftArray(fs, compose);
}

export {
  flatMapList ,
  listToOption ,
  combinationList2 ,
  combinationIfList2 ,
  flatMapArray ,
  arrayToOption ,
  foldLeftArray ,
  foldRightArray ,
  combinationArray2 ,
  combinationArray3 ,
  combinationIfArray2 ,
  combinationIfArray3 ,
  optionOr ,
  identity ,
  eq ,
  composeU ,
  compose ,
  compose3 ,
  compose4 ,
  composeN ,
}
/* No side effect */
