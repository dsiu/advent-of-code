// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Range = require("rescript/lib/js/belt_Range.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");
var Array2D$AdventOfCode = require("../../Array2D.bs.js");

function log(prim) {
  console.log(prim);
  
}

function fromArray(xs) {
  return {
          x: Belt_Option.getExn(Belt_Array.get(xs, 0)),
          y: Belt_Option.getExn(Belt_Array.get(xs, 1))
        };
}

function make(x, y) {
  return {
          x: x,
          y: y
        };
}

var Point = {
  fromArray: fromArray,
  make: make
};

function fromArray$1(xs) {
  return {
          start: fromArray(Belt_Option.getExn(Belt_Array.get(xs, 0))),
          end: fromArray(Belt_Option.getExn(Belt_Array.get(xs, 1)))
        };
}

function toPoints(t) {
  var start = t.start;
  var end = t.end;
  if (start.x === end.x) {
    var a = Utils$AdventOfCode.minIntInArray([
          start.y,
          end.y
        ]);
    var b = Utils$AdventOfCode.maxIntInArray([
          start.y,
          end.y
        ]);
    var points = {
      contents: []
    };
    Belt_Range.forEach(a, b, (function (i) {
            points.contents = Belt_Array.concat(points.contents, [{
                    x: start.x,
                    y: i
                  }]);
            
          }));
    return points.contents;
  }
  if (start.y === end.y) {
    var a$1 = Utils$AdventOfCode.minIntInArray([
          start.x,
          end.x
        ]);
    var b$1 = Utils$AdventOfCode.maxIntInArray([
          start.x,
          end.x
        ]);
    var points$1 = {
      contents: []
    };
    Belt_Range.forEach(a$1, b$1, (function (i) {
            points$1.contents = Belt_Array.concat(points$1.contents, [{
                    x: i,
                    y: start.y
                  }]);
            
          }));
    return points$1.contents;
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function onlyVertOrHoriz(t) {
  var start = t.start;
  var end = t.end;
  if (start.x === end.x) {
    return true;
  } else {
    return start.y === end.y;
  }
}

var Line = {
  fromArray: fromArray$1,
  toPoints: toPoints,
  onlyVertOrHoriz: onlyVertOrHoriz
};

function make$1(__x) {
  return Belt_Array.map(__x, fromArray$1);
}

function size(t) {
  return Belt_Array.reduce(t, {
              x: 0,
              y: 0
            }, (function (a, l) {
                var start = l.start;
                var end = l.end;
                return {
                        x: Utils$AdventOfCode.maxIntInArray([
                              a.x,
                              start.x,
                              end.x
                            ]),
                        y: Utils$AdventOfCode.maxIntInArray([
                              a.y,
                              start.y,
                              end.y
                            ])
                      };
              }));
}

var Lines = {
  make: make$1,
  size: size
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (l) {
                return Belt_Array.map(l.trim().split(" -> "), (function (x) {
                              return Belt_Array.map(x.split(","), (function (x) {
                                            return Belt_Option.getExn(Belt_Int.fromString(x));
                                          }));
                            }));
              }));
}

function solvePart1(data) {
  var lines = Belt_Array.map(parse(data), fromArray$1);
  var size$1 = size(lines);
  Array2D$AdventOfCode.make([
        size$1.x + 1 | 0,
        size$1.y + 1 | 0
      ], 0);
  Belt_Array.keepMap(lines, (function (x) {
          if (onlyVertOrHoriz(x)) {
            return toPoints(x);
          }
          
        }));
  return 1;
}

function solvePart2(data) {
  return 2;
}

exports.log = log;
exports.Point = Point;
exports.Line = Line;
exports.Lines = Lines;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
