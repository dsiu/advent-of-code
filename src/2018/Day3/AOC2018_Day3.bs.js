// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_MapInt = require("rescript/lib/js/belt_MapInt.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_format = require("rescript/lib/js/caml_format.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_MutableMapInt = require("rescript/lib/js/belt_MutableMapInt.js");
var AOC2018_Day3_Data$AdventOfCode = require("./AOC2018_Day3_Data.bs.js");

function id(t) {
  return t.id;
}

function x(t) {
  return t.x;
}

function y(t) {
  return t.y;
}

function w(t) {
  return t.w;
}

function h(t) {
  return t.h;
}

function maxX(t) {
  return t.x + t.w | 0;
}

function maxY(t) {
  return t.y + t.h | 0;
}

var claimRe = /#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i;

function parseLine(s) {
  var x = claimRe.exec(s.trim());
  if (x !== null) {
    return Belt_Array.map(x, (function (x) {
                  return Belt_Option.getExn((x == null) ? undefined : Caml_option.some(x));
                }));
  } else {
    return [];
  }
}

function make(id, x, y, w, h) {
  return {
          id: id,
          x: x,
          y: y,
          w: w,
          h: h
        };
}

function makeClaim(l) {
  var xs = parseLine(l);
  return make(Caml_format.caml_int_of_string(Belt_Array.getExn(xs, 1)), Caml_format.caml_int_of_string(Belt_Array.getExn(xs, 2)), Caml_format.caml_int_of_string(Belt_Array.getExn(xs, 3)), Caml_format.caml_int_of_string(Belt_Array.getExn(xs, 4)), Caml_format.caml_int_of_string(Belt_Array.getExn(xs, 5)));
}

var Claim = {
  id: id,
  x: x,
  y: y,
  w: w,
  h: h,
  maxX: maxX,
  maxY: maxY,
  parseLine: parseLine,
  make: make,
  makeClaim: makeClaim
};

function findMax(t, f) {
  return Belt_Array.reduce(t, 0, (function (acc, x) {
                if (Curry._1(f, x) > acc) {
                  return Curry._1(f, x);
                } else {
                  return acc;
                }
              }));
}

function findMaxX(t) {
  return findMax(t, maxX);
}

function findMaxY(t) {
  return findMax(t, maxY);
}

function make$1(lines) {
  return lines.map(makeClaim);
}

var Claims = {
  findMax: findMax,
  findMaxX: findMaxX,
  findMaxY: findMaxY,
  make: make$1
};

function w$1(t) {
  return t.w;
}

function h$1(t) {
  return t.h;
}

function matrix(t) {
  return t.matrix;
}

function make$2(w, h) {
  return {
          w: w,
          h: h,
          matrix: Belt_Array.reduce(Belt_Array.range(0, w), undefined, (function (acc, i) {
                  return Belt_MapInt.set(acc, i, Belt_MutableMapInt.make(undefined));
                }))
        };
}

function dump(t) {
  return Belt_MapInt.forEach(t.matrix, (function (x, col) {
                return Belt_MutableMapInt.forEach(col, (function (y, vs) {
                              console.log("x:" + String(x) + " y:" + String(y));
                              return Belt_Array.forEach(vs, (function (v) {
                                            console.log("  " + String(v));
                                            
                                          }));
                            }));
              }));
}

function twoOrMore(x) {
  return x >= 2;
}

function oneOrMore(x) {
  return x >= 1;
}

function isOne(x) {
  return x === 1;
}

function addPoint(t, x, y, p) {
  Belt_MutableMapInt.update(Belt_Option.getExn(Belt_MapInt.get(t.matrix, x)), y, (function (a) {
          if (a !== undefined) {
            return Belt_Array.concat(a, [p]);
          } else {
            return [p];
          }
        }));
  return t;
}

function getPoint(t, x, y) {
  return Belt_MutableMapInt.get(Belt_Option.getExn(Belt_MapInt.get(t.matrix, x)), y);
}

function fill(t, f) {
  return Belt_Array.reduce(Belt_Array.range(0, t.w), t, (function (acc, x) {
                return Belt_Array.reduce(Belt_Array.range(0, t.h), t, (function (acc, y) {
                              return addPoint(acc, x, y, Curry._2(f, x, y));
                            }));
              }));
}

function claimAreaIter(c, t, f) {
  return Belt_Array.reduce(Belt_Array.range(c.x, (c.x + c.w | 0) - 1 | 0), t, (function (acc, x) {
                return Belt_Array.reduce(Belt_Array.range(c.y, (c.y + c.h | 0) - 1 | 0), t, (function (acc, y) {
                              return Curry._4(f, acc, x, y, c);
                            }));
              }));
}

function addClaimIdToPoint(t, x, y, c) {
  return addPoint(t, x, y, c.id);
}

function addClaim(t, c) {
  return claimAreaIter(c, t, addClaimIdToPoint);
}

function getClaimIdFromPointIf(t, c, x, y, c$1) {
  var point = getPoint(t, x, y);
  var len = Belt_Option.getExn(point).length;
  if (len === 1) {
    return Caml_option.some(point);
  }
  
}

function getClaimIdsFromArea(t, c) {
  var cids = Belt_Array.concatMany(Belt_Array.reduce(Belt_Array.range(c.x, (c.x + c.w | 0) - 1 | 0), [], (function (accX, x) {
              return Belt_Array.concat(accX, Belt_Array.reduce(Belt_Array.range(c.y, (c.y + c.h | 0) - 1 | 0), [], (function (accY, y) {
                                var p = getPoint(t, x, y);
                                if (p !== undefined) {
                                  return Belt_Array.concat(accY, [p]);
                                } else {
                                  return accY;
                                }
                              })));
            })));
  if (Math.imul(c.w, c.h) === cids.length) {
    return Caml_option.some(Belt_Array.get(cids, 0));
  }
  
}

function countNonOverlapClaim(t, xs) {
  var reducer = function (param, param$1) {
    var cid = getClaimIdsFromArea(t, param$1);
    if (cid !== undefined) {
      return Belt_Array.concat(param, [Caml_option.valFromOption(cid)]);
    } else {
      return param;
    }
  };
  return Belt_Array.reduce(xs, [], reducer);
}

function countOverlap(t, p) {
  return Belt_MapInt.reduce(t.matrix, 0, (function (acc, x, col) {
                return acc + Belt_MutableMapInt.reduce(col, 0, (function (acc, y, vs) {
                              if (Curry._1(p, vs.length)) {
                                return acc + 1 | 0;
                              } else {
                                return acc;
                              }
                            })) | 0;
              }));
}

var Fabric = {
  w: w$1,
  h: h$1,
  matrix: matrix,
  make: make$2,
  dump: dump,
  twoOrMore: twoOrMore,
  oneOrMore: oneOrMore,
  isOne: isOne,
  addPoint: addPoint,
  getPoint: getPoint,
  fill: fill,
  claimAreaIter: claimAreaIter,
  addClaimIdToPoint: addClaimIdToPoint,
  addClaim: addClaim,
  getClaimIdFromPointIf: getClaimIdFromPointIf,
  getClaimIdsFromArea: getClaimIdsFromArea,
  countNonOverlapClaim: countNonOverlapClaim,
  countOverlap: countOverlap
};

function solvePart1(param) {
  var lines = AOC2018_Day3_Data$AdventOfCode.data.split("\n");
  var allClaims = lines.map(makeClaim);
  var fab = make$2(findMax(allClaims, maxX), findMax(allClaims, maxY));
  var fab$1 = Belt_Array.reduce(allClaims, fab, (function (acc, i) {
          return claimAreaIter(i, acc, addClaimIdToPoint);
        }));
  return countOverlap(fab$1, twoOrMore);
}

function solvePart2(param) {
  var lines = AOC2018_Day3_Data$AdventOfCode.data.split("\n");
  var allClaims = lines.map(makeClaim);
  var fab = make$2(findMax(allClaims, maxX), findMax(allClaims, maxY));
  var fab$1 = Belt_Array.reduce(allClaims, fab, (function (acc, i) {
          return claimAreaIter(i, acc, addClaimIdToPoint);
        }));
  return countNonOverlapClaim(fab$1, allClaims);
}

var allClaims = [
    "#3 @ 1,3: 4x4",
    "#7 @ 3,1: 4x4",
    "#11 @ 5,5: 2x2"
  ].map(makeClaim);

var w$2 = findMax(allClaims, maxX);

var h$2 = findMax(allClaims, maxY);

var test_fab = make$2(w$2, h$2);

var test_fab$1 = Belt_Array.reduce(allClaims, test_fab, (function (acc, i) {
        return claimAreaIter(i, acc, addClaimIdToPoint);
      }));

var solvePart2Demo = undefined === (countNonOverlapClaim(test_fab$1, allClaims), undefined);

var data = AOC2018_Day3_Data$AdventOfCode.data;

exports.data = data;
exports.Claim = Claim;
exports.Claims = Claims;
exports.Fabric = Fabric;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
exports.solvePart2Demo = solvePart2Demo;
/* solvePart2Demo Not a pure module */
