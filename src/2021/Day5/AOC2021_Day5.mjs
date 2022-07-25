// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Range from "rescript/lib/es6/belt_Range.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";

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

function getLine(t) {
  return t._0;
}

function toString(t) {
  var line = t._0;
  var start = line.start;
  var end = line.end;
  var tmp;
  switch (t.TAG | 0) {
    case /* Horizontal */0 :
        tmp = "Horizontal";
        break;
    case /* Vertical */1 :
        tmp = "Vertical";
        break;
    case /* Diagonal */2 :
        tmp = "Diagonal";
        break;
    case /* Other */3 :
        tmp = "Other";
        break;
    
  }
  return tmp + (" : (" + String(start.x) + "," + String(start.y) + "), (" + String(end.x) + "," + String(end.y) + ")");
}

function fromArray$1(xs) {
  var start = fromArray(Belt_Option.getExn(Belt_Array.get(xs, 0)));
  var end = fromArray(Belt_Option.getExn(Belt_Array.get(xs, 1)));
  if (start.x === end.x) {
    return {
            TAG: /* Vertical */1,
            _0: {
              start: start,
              end: end
            }
          };
  } else if (start.y === end.y) {
    return {
            TAG: /* Horizontal */0,
            _0: {
              start: start,
              end: end
            }
          };
  } else if (Math.abs(start.x - end.x | 0) === Math.abs(start.y - end.y | 0)) {
    return {
            TAG: /* Diagonal */2,
            _0: {
              start: start,
              end: end
            }
          };
  } else {
    return {
            TAG: /* Other */3,
            _0: {
              start: start,
              end: end
            }
          };
  }
}

function toPoints2(t) {
  switch (t.TAG | 0) {
    case /* Horizontal */0 :
        var l = t._0;
        var start = l.start;
        var end = l.end;
        var a = Utils$AdventOfCode.minIntInArray([
              start.x,
              end.x
            ]);
        var b = Utils$AdventOfCode.maxIntInArray([
              start.x,
              end.x
            ]);
        var points = {
          contents: []
        };
        Belt_Range.forEach(a, b, (function (i) {
                points.contents = Belt_Array.concat(points.contents, [{
                        x: i,
                        y: start.y
                      }]);
                
              }));
        return points.contents;
    case /* Vertical */1 :
        var l$1 = t._0;
        var start$1 = l$1.start;
        var end$1 = l$1.end;
        var a$1 = Utils$AdventOfCode.minIntInArray([
              start$1.y,
              end$1.y
            ]);
        var b$1 = Utils$AdventOfCode.maxIntInArray([
              start$1.y,
              end$1.y
            ]);
        var points$1 = {
          contents: []
        };
        Belt_Range.forEach(a$1, b$1, (function (i) {
                points$1.contents = Belt_Array.concat(points$1.contents, [{
                        x: start$1.x,
                        y: i
                      }]);
                
              }));
        return points$1.contents;
    case /* Diagonal */2 :
        return [];
    case /* Other */3 :
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
    
  }
}

function cmp(a, b) {
  if (a === b) {
    return 0;
  } else if (Caml_obj.caml_greaterthan(a, b)) {
    return 1;
  } else {
    return -1;
  }
}

function makeRange(start, end) {
  var match = cmp(end, start);
  switch (match) {
    case -1 :
        return Belt_Array.reverse(Belt_Array.range(end, start));
    case 0 :
        return [];
    case 1 :
        return Belt_Array.range(start, end);
    default:
      return [];
  }
}

function makePoints(start, end) {
  var diff_x = Math.abs(end.x - start.x | 0);
  var diff_y = Math.abs(end.y - start.y | 0);
  var xs = start.x !== end.x ? makeRange(start.x, end.x) : Belt_Array.make(diff_y + 1 | 0, start.x);
  var ys = start.y !== end.y ? makeRange(start.y, end.y) : Belt_Array.make(diff_x + 1 | 0, start.y);
  return Belt_Array.zipBy(xs, ys, (function (x, y) {
                return {
                        x: x,
                        y: y
                      };
              }));
}

function toPoints(t) {
  if (t.TAG === /* Other */3) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  var l = t._0;
  var start = l.start;
  var end = l.end;
  return makePoints(start, end);
}

function onlyVertOrHoriz(t) {
  switch (t.TAG | 0) {
    case /* Horizontal */0 :
    case /* Vertical */1 :
        return true;
    case /* Diagonal */2 :
    case /* Other */3 :
        return false;
    
  }
}

function onlyVertOrHorizOrDiagonal(t) {
  if (t.TAG === /* Other */3) {
    return false;
  } else {
    return true;
  }
}

var Line = {
  getLine: getLine,
  toString: toString,
  fromArray: fromArray$1,
  toPoints2: toPoints2,
  cmp: cmp,
  makeRange: makeRange,
  makePoints: makePoints,
  toPoints: toPoints,
  onlyVertOrHoriz: onlyVertOrHoriz,
  onlyVertOrHorizOrDiagonal: onlyVertOrHorizOrDiagonal
};

function make$1(__x) {
  return Belt_Array.map(__x, fromArray$1);
}

function size(t) {
  return Belt_Array.reduce(t, {
              x: 0,
              y: 0
            }, (function (a, l) {
                var line = l._0;
                var start = line.start;
                var end = line.end;
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

function update(t, param, fn) {
  var y = param[1];
  var x = param[0];
  return Array2D$AdventOfCode.set(t, [
              x,
              y
            ], Curry._1(fn, Array2D$AdventOfCode.getExn(t, [
                      x,
                      y
                    ])));
}

function count(t, fn) {
  return Belt_Array.reduce(t, 0, (function (a, y) {
                return a + Belt_Array.reduce(y, 0, (function (a, x) {
                              if (Curry._1(fn, x)) {
                                return a + 1 | 0;
                              } else {
                                return a;
                              }
                            })) | 0;
              }));
}

var Vents = {
  make: Array2D$AdventOfCode.make,
  update: update,
  count: count
};

function solve(data, filter) {
  var lines = Belt_Array.map(parse(data), fromArray$1);
  var size$1 = size(lines);
  var points = Belt_Array.keepMap(lines, (function (x) {
          if (Curry._1(filter, x)) {
            return toPoints(x);
          }
          
        }));
  var add1 = function (__x) {
    return Utils$AdventOfCode.add(__x, 1);
  };
  var vents = Array2D$AdventOfCode.make([
        size$1.x + 1 | 0,
        size$1.y + 1 | 0
      ], 0);
  Belt_Array.forEach(Utils$AdventOfCode.flatten(points), (function (p) {
          update(vents, [
                p.x,
                p.y
              ], add1);
          
        }));
  return count(vents, (function (x) {
                return x >= 2;
              }));
}

function solvePart1(__x) {
  return solve(__x, onlyVertOrHoriz);
}

function solvePart2(__x) {
  return solve(__x, onlyVertOrHorizOrDiagonal);
}

export {
  log ,
  Point ,
  Line ,
  Lines ,
  parse ,
  Vents ,
  solve ,
  solvePart1 ,
  solvePart2 ,
  
}
/* No side effect */