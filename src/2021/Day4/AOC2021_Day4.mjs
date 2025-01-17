// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_SetInt from "rescript/lib/es6/belt_SetInt.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";

function log(prim) {
  console.log(prim);
}

function make(line) {
  return Belt_Array.keepMap((function (__x) {
                  return __x.split(",");
                })(line), Belt_Int.fromString);
}

var Draws = {
  make: make
};

function toString(t) {
  return Array2D$AdventOfCode.toString(t, (function (__x) {
                return __x.toString();
              }));
}

function make$1(lines) {
  return Belt_Array.map(lines, (function (x) {
                return Belt_Array.keepMap(x.split(" "), (function (s) {
                              if (s.length > 0) {
                                return Belt_Int.fromString(s.trim());
                              }
                              
                            }));
              }));
}

function match(candidates, match_draws) {
  var can_set = Belt_SetInt.fromArray(candidates);
  var draws_set = Belt_SetInt.fromArray(match_draws);
  if (Belt_SetInt.subset(can_set, draws_set)) {
    return Belt_SetInt.toArray(can_set);
  }
  
}

function solve(t, match_draws) {
  var helper = function (t, i, getter, limit) {
    if (i < limit) {
      return Belt_Option.flatMap(getter(t, i), (function (row) {
                    var matched = match(row, match_draws);
                    if (matched !== undefined) {
                      return matched;
                    } else {
                      return helper(t, i + 1 | 0, getter, limit);
                    }
                  }));
    }
    
  };
  var matchY = helper(t, 0, Array2D$AdventOfCode.getYEquals, Array2D$AdventOfCode.lengthY(t));
  var matchX = helper(t, 0, Array2D$AdventOfCode.getXEquals, Array2D$AdventOfCode.lengthX(t));
  var remove_marked = function (t, m) {
    return Belt_SetInt.toArray(Belt_SetInt.diff(Belt_SetInt.fromArray(Array2D$AdventOfCode.flatten(t)), Belt_SetInt.fromArray(m)));
  };
  if (matchX !== undefined || matchY !== undefined) {
    return remove_marked(t, match_draws);
  }
  
}

var Board = {
  toString: toString,
  make: make$1,
  match: match,
  solve: solve
};

function make$2(lines) {
  return Belt_Array.map(lines, (function (b) {
                return make$1(Belt_Array.map((function (__x) {
                                    return Utils$AdventOfCode.splitNewline(__x);
                                  })(b), (function (prim) {
                                  return prim.trim();
                                })));
              }));
}

function toString$1(t) {
  return Utils$AdventOfCode.Printable.$$Array.toString(t, toString);
}

function solvePart1(t, draws) {
  var _i = 5;
  var limit = draws.length;
  while(true) {
    var i = _i;
    if (i > limit) {
      return ;
    }
    var keys = Belt_Array.slice(draws, 0, i);
    var results = Belt_Array.keepMap(t, (function(keys){
        return function (__x) {
          return solve(__x, keys);
        }
        }(keys)));
    var match = results.length;
    if (match !== 0) {
      if (match !== 1) {
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      return [
              Belt_Option.getExn(Belt_Array.get(draws, i - 1 | 0)),
              Belt_Array.get(results, 0)
            ];
    }
    _i = i + 1 | 0;
    continue ;
  };
}

function solvePart2(t, draws) {
  var _t = t;
  var _i = 5;
  var limit = draws.length;
  while(true) {
    var i = _i;
    var t$1 = _t;
    if (i > limit) {
      return ;
    }
    var keys = Belt_Array.slice(draws, 0, i);
    var results = Belt_Array.keepMap(t$1, (function(keys){
        return function (__x) {
          return solve(__x, keys);
        }
        }(keys)));
    var results_not = Belt_Array.keepMap(t$1, (function(keys){
        return function (b) {
          var match = solve(b, keys);
          if (match !== undefined) {
            return ;
          } else {
            return b;
          }
        }
        }(keys)));
    var match = results.length;
    var match$1 = results_not.length;
    if (match === 1 && match$1 === 0) {
      return [
              Belt_Option.getExn(Belt_Array.get(draws, i - 1 | 0)),
              Belt_Array.get(results, 0)
            ];
    }
    _i = i + 1 | 0;
    _t = results_not;
    continue ;
  };
}

var Boards = {
  make: make$2,
  toString: toString$1,
  solvePart1: solvePart1,
  solvePart2: solvePart2
};

function parse(data) {
  var lines = Belt_Array.map(Utils$AdventOfCode.splitDoubleNewline(data), (function (prim) {
          return prim.trim();
        }));
  var draws = make(Belt_Option.getExn(Belt_Array.get(lines, 0)));
  var boards = make$2(Belt_Array.sliceToEnd(lines, 1));
  return [
          draws,
          boards
        ];
}

function solvePart1$1(data) {
  var match = parse(data);
  var match$1 = Belt_Option.getExn(solvePart1(match[1], match[0]));
  return Math.imul(match$1[0], Belt_Array.reduce(Belt_Option.getExn(match$1[1]), 0, Utils$AdventOfCode.add));
}

function solvePart2$1(data) {
  var match = parse(data);
  var match$1 = Belt_Option.getExn(solvePart2(match[1], match[0]));
  return Math.imul(match$1[0], Belt_Array.reduce(Belt_Option.getExn(match$1[1]), 0, Utils$AdventOfCode.add));
}

export {
  log ,
  Draws ,
  Board ,
  Boards ,
  parse ,
  solvePart1$1 as solvePart1,
  solvePart2$1 as solvePart2,
}
/* Utils-AdventOfCode Not a pure module */
