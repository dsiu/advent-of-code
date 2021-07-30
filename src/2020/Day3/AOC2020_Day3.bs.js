// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Array2D$AdventOfCode = require("../../Array2D.bs.js");

function log(prim) {
  console.log(prim);
  
}

function height(t) {
  return Belt_Option.getExn(Belt_Array.get(t, 0)).length;
}

function width(t) {
  return t.length;
}

function get(t, param) {
  return Array2D$AdventOfCode.get(t, [
              param[0],
              param[1]
            ]);
}

function getWrapped(t, param) {
  var w = t.length;
  var h = height(t);
  var newX = Caml_int32.mod_(param[0], w);
  var newY = Caml_int32.mod_(param[1], h);
  return Array2D$AdventOfCode.get(t, [
              newX,
              newY
            ]);
}

function isTree(c) {
  return c === "#";
}

function make(xs) {
  var x = Belt_Array.getExn(xs, 0).length;
  var y = xs.length;
  var ret = Array2D$AdventOfCode.make([
        x,
        y
      ], "");
  Belt_Array.forEachWithIndex(xs, (function (j, s) {
          var ss = s.split("");
          return Belt_Array.forEachWithIndex(ss, (function (i, c) {
                        Array2D$AdventOfCode.set(ret, [
                              i,
                              j
                            ], c);
                        
                      }));
        }));
  return ret;
}

function walk(t, param) {
  var down = param[1];
  var side = param[0];
  var h = height(t);
  var _count = 0;
  var _param = [
    0,
    0
  ];
  while(true) {
    var param$1 = _param;
    var count = _count;
    var y = param$1[1];
    var x = param$1[0];
    var nextX = x + side | 0;
    var nextY = y + down | 0;
    var cur = Belt_Option.getExn(getWrapped(t, [
              x,
              y
            ])) === "#";
    var done = y >= h;
    if (done) {
      return count;
    }
    if (cur) {
      _param = [
        nextX,
        nextY
      ];
      _count = count + 1 | 0;
      continue ;
    }
    _param = [
      nextX,
      nextY
    ];
    continue ;
  };
}

var TreeMap = {
  height: height,
  width: width,
  get: get,
  getWrapped: getWrapped,
  isTree: isTree,
  make: make,
  walk: walk
};

function solvePart1(data) {
  var parsed = Belt_Array.map(data.split("\n"), (function (prim) {
          return prim.trim();
        }));
  var __x = make(parsed);
  return walk(__x, [
              3,
              1
            ]);
}

function solvePart2(data) {
  var parsed = Belt_Array.map(data.split("\n"), (function (prim) {
          return prim.trim();
        }));
  var tm = make(parsed);
  var slopes = [
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
  var trees = Belt_Array.map(slopes, (function (s) {
          return walk(tm, s);
        }));
  var acc = BigInt(1);
  return Belt_Array.reduce(trees, acc, (function (a, s) {
                  var ss = BigInt(s);
                  return a * ss;
                })).toString();
}

exports.log = log;
exports.TreeMap = TreeMap;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
