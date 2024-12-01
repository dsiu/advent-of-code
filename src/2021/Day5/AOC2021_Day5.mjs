// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Range from "rescript/lib/es6/Belt_Range.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
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

let Point = {
  fromArray: fromArray,
  make: make
};

function getLine(t) {
  return t._0;
}

function toString(t) {
  let line = t._0;
  let start = line.start;
  let end = line.end;
  let tmp;
  switch (t.TAG) {
    case "Horizontal" :
      tmp = "Horizontal";
      break;
    case "Vertical" :
      tmp = "Vertical";
      break;
    case "Diagonal" :
      tmp = "Diagonal";
      break;
    case "Other" :
      tmp = "Other";
      break;
  }
  return tmp + (" : (" + String(start.x) + "," + String(start.y) + "), (" + String(end.x) + "," + String(end.y) + ")");
}

function fromArray$1(xs) {
  let start = fromArray(Belt_Option.getExn(Belt_Array.get(xs, 0)));
  let end = fromArray(Belt_Option.getExn(Belt_Array.get(xs, 1)));
  if (start.x === end.x) {
    return {
      TAG: "Vertical",
      _0: {
        start: start,
        end: end
      }
    };
  } else if (start.y === end.y) {
    return {
      TAG: "Horizontal",
      _0: {
        start: start,
        end: end
      }
    };
  } else if (Math.abs(start.x - end.x | 0) === Math.abs(start.y - end.y | 0)) {
    return {
      TAG: "Diagonal",
      _0: {
        start: start,
        end: end
      }
    };
  } else {
    return {
      TAG: "Other",
      _0: {
        start: start,
        end: end
      }
    };
  }
}

function toPoints2(t) {
  switch (t.TAG) {
    case "Horizontal" :
      let l = t._0;
      let start = l.start;
      let end = l.end;
      let a = Utils$AdventOfCode.minIntInArray([
        start.x,
        end.x
      ]);
      let b = Utils$AdventOfCode.maxIntInArray([
        start.x,
        end.x
      ]);
      let points = {
        contents: []
      };
      Belt_Range.forEach(a, b, i => {
        points.contents = Belt_Array.concat(points.contents, [{
            x: i,
            y: start.y
          }]);
      });
      return points.contents;
    case "Vertical" :
      let l$1 = t._0;
      let start$1 = l$1.start;
      let end$1 = l$1.end;
      let a$1 = Utils$AdventOfCode.minIntInArray([
        start$1.y,
        end$1.y
      ]);
      let b$1 = Utils$AdventOfCode.maxIntInArray([
        start$1.y,
        end$1.y
      ]);
      let points$1 = {
        contents: []
      };
      Belt_Range.forEach(a$1, b$1, i => {
        points$1.contents = Belt_Array.concat(points$1.contents, [{
            x: start$1.x,
            y: i
          }]);
      });
      return points$1.contents;
    case "Diagonal" :
      return [];
    case "Other" :
      throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
  }
}

function cmp(a, b) {
  if (a === b) {
    return 0;
  } else if (Primitive_object.greaterthan(a, b)) {
    return 1;
  } else {
    return -1;
  }
}

function makeRange(start, end) {
  let match = cmp(end, start);
  switch (match) {
    case -1 :
      return Belt_Array.reverse(Belt_Array.range(end, start));
    case 1 :
      return Belt_Array.range(start, end);
    default:
      return [];
  }
}

function makePoints(start, end) {
  let diff_x = Math.abs(end.x - start.x | 0);
  let diff_y = Math.abs(end.y - start.y | 0);
  let xs = start.x !== end.x ? makeRange(start.x, end.x) : Belt_Array.make(diff_y + 1 | 0, start.x);
  let ys = start.y !== end.y ? makeRange(start.y, end.y) : Belt_Array.make(diff_x + 1 | 0, start.y);
  return Belt_Array.zipBy(xs, ys, (x, y) => ({
    x: x,
    y: y
  }));
}

function toPoints(t) {
  if (t.TAG === "Other") {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  }
  let l = t._0;
  let start = l.start;
  let end = l.end;
  return makePoints(start, end);
}

function onlyVertOrHoriz(t) {
  switch (t.TAG) {
    case "Horizontal" :
    case "Vertical" :
      return true;
    case "Diagonal" :
    case "Other" :
      return false;
  }
}

function onlyVertOrHorizOrDiagonal(t) {
  return t.TAG !== "Other";
}

let Line = {
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
  }, (a, l) => {
    let line = l._0;
    let start = line.start;
    let end = line.end;
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
  });
}

let Lines = {
  make: make$1,
  size: size
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), l => Belt_Array.map(l.trim().split(" -> "), x => Belt_Array.map(x.split(","), Utils$AdventOfCode.intFromStringExn)));
}

function update(t, param, fn) {
  let y = param[1];
  let x = param[0];
  Array2D$AdventOfCode.set(t, [
    x,
    y
  ], fn(Array2D$AdventOfCode.getExn(t, [
    x,
    y
  ])));
}

function count(t, fn) {
  return Belt_Array.reduce(t, 0, (a, y) => a + Belt_Array.reduce(y, 0, (a, x) => {
    if (fn(x)) {
      return a + 1 | 0;
    } else {
      return a;
    }
  }) | 0);
}

let Vents = {
  make: Array2D$AdventOfCode.make,
  update: update,
  count: count
};

function solve(data, filter) {
  let lines = Belt_Array.map(parse(data), fromArray$1);
  let size$1 = size(lines);
  let points = Belt_Array.keepMap(lines, x => {
    if (filter(x)) {
      return toPoints(x);
    }
    
  });
  let add1 = __x => Utils$AdventOfCode.add(__x, 1);
  let vents = Array2D$AdventOfCode.make([
    size$1.x + 1 | 0,
    size$1.y + 1 | 0
  ], 0);
  Belt_Array.forEach(Utils$AdventOfCode.flatten(points), p => {
    update(vents, [
      p.x,
      p.y
    ], add1);
  });
  return count(vents, x => x >= 2);
}

function solvePart1(__x) {
  return solve(__x, onlyVertOrHoriz);
}

function solvePart2(__x) {
  return solve(__x, onlyVertOrHorizOrDiagonal);
}

export {
  log,
  Point,
  Line,
  Lines,
  parse,
  Vents,
  solve,
  solvePart1,
  solvePart2,
}
/* Utils-AdventOfCode Not a pure module */
