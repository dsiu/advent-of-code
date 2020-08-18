// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Belt_MutableMapInt = require("bs-platform/lib/js/belt_MutableMapInt.js");
var Day3_Data$AdventOfCode = require("./Day3_Data.bs.js");

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

function make(id, x, y, w, h) {
  return {
          id: id,
          x: x,
          y: y,
          w: w,
          h: h
        };
}

var Claim = {
  id: id,
  x: x,
  y: y,
  w: w,
  h: h,
  maxX: maxX,
  maxY: maxY,
  make: make
};

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

function makeClaim(x) {
  var xs = parseLine(x);
  return make(Caml_format.caml_int_of_string(Belt_Option.getExn(Belt_Array.get(xs, 1))), Caml_format.caml_int_of_string(Belt_Option.getExn(Belt_Array.get(xs, 2))), Caml_format.caml_int_of_string(Belt_Option.getExn(Belt_Array.get(xs, 3))), Caml_format.caml_int_of_string(Belt_Option.getExn(Belt_Array.get(xs, 4))), Caml_format.caml_int_of_string(Belt_Option.getExn(Belt_Array.get(xs, 5))));
}

function allClaim(lines) {
  return lines.map(makeClaim);
}

function findMax(xs, f) {
  return Belt_Array.reduce(xs, 0, (function (acc, x) {
                if (Curry._1(f, x) > acc) {
                  return Curry._1(f, x);
                } else {
                  return acc;
                }
              }));
}

function findMaxX(__x) {
  return findMax(__x, maxX);
}

function findMaxY(__x) {
  return findMax(__x, maxY);
}

function w$1(t) {
  return t.w;
}

function h$1(t) {
  return t.h;
}

function matrix(t) {
  return t.matrix;
}

function make$1(w, h) {
  return {
          w: w,
          h: h,
          matrix: Belt_Array.reduce(Belt_Array.range(0, h), undefined, (function (acc, i) {
                  return Belt_MapInt.set(acc, i, Belt_MutableMapInt.make(undefined));
                }))
        };
}

function addPoint(t, x, y, p) {
  return Belt_MutableMapInt.update(Belt_Option.getExn(Belt_MapInt.get(t.matrix, x)), x, (function (a) {
                if (a !== undefined) {
                  return Belt_Array.concat(a, [p]);
                } else {
                  return [p];
                }
              }));
}

function getPoint(t, x, y) {
  return Belt_Option.getExn(Belt_MutableMapInt.get(Belt_Option.getExn(Belt_MapInt.get(t.matrix, x)), y));
}

var Fabric = {
  w: w$1,
  h: h$1,
  matrix: matrix,
  make: make$1,
  addPoint: addPoint,
  getPoint: getPoint
};

var lines = Day3_Data$AdventOfCode.data.split("\n");

var __x = lines.map(makeClaim);

var size_x = findMax(__x, maxX);

var lines$1 = Day3_Data$AdventOfCode.data.split("\n");

var __x$1 = lines$1.map(makeClaim);

var size_y = findMax(__x$1, maxY);

var fab = make$1(size_x, size_y);

var data = Day3_Data$AdventOfCode.data;

exports.data = data;
exports.Claim = Claim;
exports.claimRe = claimRe;
exports.parseLine = parseLine;
exports.makeClaim = makeClaim;
exports.allClaim = allClaim;
exports.findMax = findMax;
exports.findMaxX = findMaxX;
exports.findMaxY = findMaxY;
exports.Fabric = Fabric;
exports.size_x = size_x;
exports.size_y = size_y;
exports.fab = fab;
/* lines Not a pure module */
