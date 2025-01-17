// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Caml_splice_call from "rescript/lib/es6/caml_splice_call.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function isVisible(param) {
  return param._1;
}

function treeHeight(param) {
  return param._0;
}

function setVisibility(row) {
  var vis = function (param, param$1) {
    var height = param$1._0;
    var tagged = param[1];
    var highest = param[0];
    if (height > highest) {
      return [
              height,
              [{
                    TAG: "Tree",
                    _0: height,
                    _1: true
                  }].concat(tagged)
            ];
    } else {
      return [
              highest,
              [{
                    TAG: "Tree",
                    _0: height,
                    _1: param$1._1
                  }].concat(tagged)
            ];
    }
  };
  return Stdlib__Array.reduce(row, [
                  -1,
                  []
                ], vis)[1].toReversed();
}

function setVisibilityOrient(__x) {
  return __x.map(setVisibility);
}

function setVisibilityForest(forest) {
  var rotate = function (x) {
    return Utils$AdventOfCode.transpose(x).map(function (prim) {
                return prim.toReversed();
              });
  };
  var f = Utils$AdventOfCode.compose(setVisibilityOrient, rotate);
  return f(f(f(f(forest))));
}

function countVisible(forest) {
  return (function (__x) {
                return Caml_splice_call.spliceObjApply([], "concat", [__x]);
              })(forest).filter(isVisible).length;
}

var part1 = Utils$AdventOfCode.compose(setVisibilityForest, countVisible);

function tracks(forest, row, col) {
  var match = (function (__x) {
        return Stdlib__Array.splitAt(__x, col);
      })(Stdlib__Array.getUnsafe(forest, row));
  var match$1 = (function (__x) {
        return Stdlib__Array.splitAt(__x, row);
      })(Stdlib__Array.getUnsafe(Utils$AdventOfCode.transpose(forest), col));
  return [
          match[0].toReversed(),
          Stdlib__Array.drop(match[1], 1),
          match$1[0].toReversed(),
          Stdlib__Array.drop(match$1[1], 1)
        ];
}

function takeWhile1(xs, f) {
  if (xs.length === 0) {
    return [];
  }
  var h = Stdlib__Array.headUnsafe(xs);
  var t = Stdlib__Array.tail(xs);
  var match = f(h);
  if (match) {
    return [h].concat(takeWhile1(t, f));
  } else {
    return [h];
  }
}

function viewDistance(trees, h) {
  return takeWhile1(trees.map(treeHeight), (function (x) {
                return x < h;
              })).length;
}

function scenicScore(forest, row, col) {
  var directions = tracks(forest, row, col);
  var h = treeHeight(Stdlib__Array.getUnsafe(Stdlib__Array.getUnsafe(forest, row), col));
  return Stdlib__Array.reduce(directions.map(function (__x) {
                  return viewDistance(__x, h);
                }), 1, (function (a, x) {
                return Math.imul(a, x);
              }));
}

function part2(forest) {
  var id = function (prim) {
    return prim;
  };
  var nrows = forest.length;
  var ncols = Stdlib__Array.headUnsafe(forest).length;
  var scores = Stdlib__Array.combination2(Stdlib__Array.makeBy(nrows - 1 | 0, id), Stdlib__Array.makeBy(ncols - 1 | 0, id), (function (r, c) {
          return scenicScore(forest, r, c);
        }));
  return Utils$AdventOfCode.maxIntInArray(scores);
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(function (x) {
              return Utils$AdventOfCode.splitChars(x.trim()).map(function (x) {
                          return {
                                  TAG: "Tree",
                                  _0: Caml_format.int_of_string(x),
                                  _1: false
                                };
                        });
            });
}

function solvePart1(data) {
  return part1(parse(data));
}

function solvePart2(data) {
  return part2(parse(data));
}

var A;

var O;

export {
  log ,
  A ,
  O ,
  isVisible ,
  treeHeight ,
  setVisibility ,
  setVisibilityOrient ,
  setVisibilityForest ,
  countVisible ,
  part1 ,
  tracks ,
  takeWhile1 ,
  viewDistance ,
  scenicScore ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* part1 Not a pure module */
