// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Tree$AdventOfCode from "../../Tree.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function log3(prim0, prim1, prim2) {
  console.log(prim0, prim1, prim2);
}

function splittable(t) {
  var splittableC = function (loc) {
    var n = loc._0;
    if (n.TAG === "Leaf") {
      if (n._0 >= 10) {
        return loc;
      } else {
        return ;
      }
    } else {
      return Stdlib__Option.or_(splittableC(Tree$AdventOfCode.left(loc)), splittableC(Tree$AdventOfCode.right(loc)));
    }
  };
  return splittableC(Tree$AdventOfCode.top(t));
}

function split(num) {
  var mn0 = splittable(num);
  if (mn0 === undefined) {
    return ;
  }
  var sn = mn0._0;
  if (sn.TAG === "Leaf") {
    var sn$1 = sn._0;
    var ln = sn$1 / 2 | 0;
    var rn = ln + sn$1 % 2 | 0;
    var n1 = Tree$AdventOfCode.modify(mn0, (function (param) {
            return {
                    TAG: "Pair",
                    _0: {
                      TAG: "Leaf",
                      _0: ln
                    },
                    _1: {
                      TAG: "Leaf",
                      _0: rn
                    }
                  };
          }));
    var match = Tree$AdventOfCode.upmost(n1);
    return match._0;
  }
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2021_Day18.res",
          56,
          12
        ],
        Error: new Error()
      };
}

function pairAtDepthC(n, l) {
  if (l._0.TAG === "Leaf") {
    return ;
  } else if (n !== 0) {
    return Stdlib__Option.or_(pairAtDepthC(n - 1 | 0, Tree$AdventOfCode.left(l)), pairAtDepthC(n - 1 | 0, Tree$AdventOfCode.right(l)));
  } else {
    return l;
  }
}

function pairAtDepth(n, t) {
  return pairAtDepthC(n, Tree$AdventOfCode.top(t));
}

function rightmostNum(_loc) {
  while(true) {
    var loc = _loc;
    if (loc._0.TAG === "Leaf") {
      return loc;
    }
    _loc = Tree$AdventOfCode.right(loc);
    continue ;
  };
}

function rightmostOnLeft(_loc) {
  while(true) {
    var loc = _loc;
    var tmp = loc._1;
    if (typeof tmp !== "object") {
      return ;
    }
    if (tmp.TAG !== "L") {
      return rightmostNum(Tree$AdventOfCode.left(Tree$AdventOfCode.up(loc)));
    }
    _loc = Tree$AdventOfCode.up(loc);
    continue ;
  };
}

function leftmostNum(_loc) {
  while(true) {
    var loc = _loc;
    if (loc._0.TAG === "Leaf") {
      return loc;
    }
    _loc = Tree$AdventOfCode.left(loc);
    continue ;
  };
}

function leftmostOnRight(_loc) {
  while(true) {
    var loc = _loc;
    var tmp = loc._1;
    if (typeof tmp !== "object") {
      return ;
    }
    if (tmp.TAG === "L") {
      return leftmostNum(Tree$AdventOfCode.right(Tree$AdventOfCode.up(loc)));
    }
    _loc = Tree$AdventOfCode.up(loc);
    continue ;
  };
}

function explode(num) {
  var mp0 = pairAtDepth(4, num);
  if (mp0 === undefined) {
    return ;
  }
  var match = mp0._0;
  if (match.TAG !== "Leaf") {
    var nl = match._0;
    if (nl.TAG === "Leaf") {
      var nr = match._1;
      var nl$1 = nl._0;
      if (nr.TAG === "Leaf") {
        var nr$1 = nr._0;
        var leftReg = rightmostOnLeft(mp0);
        var p1 = leftReg !== undefined ? Tree$AdventOfCode.modify(leftReg, (function (n) {
                  if (n.TAG === "Leaf") {
                    return {
                            TAG: "Leaf",
                            _0: n._0 + nl$1 | 0
                          };
                  }
                  throw {
                        RE_EXN_ID: "Match_failure",
                        _1: [
                          "AOC2021_Day18.res",
                          143,
                          46
                        ],
                        Error: new Error()
                      };
                })) : mp0;
        var rightReg = Belt_Option.flatMap(pairAtDepthC(4, Tree$AdventOfCode.upmost(p1)), leftmostOnRight);
        var p2 = rightReg !== undefined ? Tree$AdventOfCode.modify(rightReg, (function (n) {
                  if (n.TAG === "Leaf") {
                    return {
                            TAG: "Leaf",
                            _0: n._0 + nr$1 | 0
                          };
                  }
                  throw {
                        RE_EXN_ID: "Match_failure",
                        _1: [
                          "AOC2021_Day18.res",
                          148,
                          48
                        ],
                        Error: new Error()
                      };
                })) : p1;
        var centrePair = pairAtDepthC(4, Tree$AdventOfCode.upmost(p2));
        var p3 = centrePair !== undefined ? Tree$AdventOfCode.modify(centrePair, (function (param) {
                  return {
                          TAG: "Leaf",
                          _0: 0
                        };
                })) : p2;
        var match$1 = Tree$AdventOfCode.upmost(p3);
        return match$1._0;
      }
      
    }
    
  }
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "AOC2021_Day18.res",
          139,
          12
        ],
        Error: new Error()
      };
}

function reduce(_num) {
  while(true) {
    var num = _num;
    var num1 = Stdlib__Option.or_(explode(num), split(num));
    if (num1 === undefined) {
      return num;
    }
    _num = num1;
    continue ;
  };
}

function snailAdd(a, b) {
  return reduce({
              TAG: "Pair",
              _0: a,
              _1: b
            });
}

function total(xs) {
  return Stdlib__Array.foldl1(xs, snailAdd);
}

function magnitude(t) {
  if (t.TAG === "Leaf") {
    return t._0;
  } else {
    return Math.imul(3, magnitude(t._0)) + (magnitude(t._1) << 1) | 0;
  }
}

function part1(numbers) {
  var total = Stdlib__Array.foldl1(numbers, snailAdd);
  return magnitude(total);
}

function part2(numbers) {
  return Utils$AdventOfCode.maxIntInArray(Stdlib__Array.combination2(numbers, numbers, (function (a, b) {
                    return magnitude(snailAdd(a, b));
                  })));
}

function charToString(c) {
  return String.fromCharCode(c);
}

function concatStringList(chars) {
  if (chars) {
    return chars.hd + concatStringList(chars.tl);
  } else {
    return "";
  }
}

var comma = Res_parser.$$char(/* ',' */44);

var zero = Res_parser.$$char(/* '0' */48);

var oneThroughNine = Res_parser.satisfy(function (c) {
      if (c >= /* '1' */49) {
        return /* '9' */57 >= c;
      } else {
        return false;
      }
    });

var digit = Res_parser.map(Res_parser.orElse(zero, oneThroughNine), charToString);

var digits = Res_parser.map(Res_parser.atLeastOne(digit), concatStringList);

var pair = Res_parser.makeRecursive(function (p) {
      var betweenBraces = function (__x) {
        return Res_parser.between(__x, Res_parser.$$char(/* '[' */91), Res_parser.$$char(/* ']' */93));
      };
      var makeIntElem = function (x) {
        return {
                TAG: "Leaf",
                _0: Utils$AdventOfCode.intFromStringExn(x)
              };
      };
      var pairOrNumber = Res_parser.choice([
            p,
            Res_parser.map(digits, makeIntElem)
          ]);
      return Res_parser.map(betweenBraces(Res_parser.andThen(Res_parser.keepLeft(pairOrNumber, comma), pairOrNumber)), (function (param) {
                    return {
                            TAG: "Pair",
                            _0: param[0],
                            _1: param[1]
                          };
                  }));
    });

function parse(s) {
  return Res_parser.run(pair, s);
}

function parseAndGetResult(s) {
  return Belt_Result.getExn(Res_parser.run(pair, s))[0];
}

var Parser = {
  charToString: charToString,
  concatStringList: concatStringList,
  comma: comma,
  zero: zero,
  oneThroughNine: oneThroughNine,
  digit: digit,
  digits: digits,
  pair: pair,
  parse: parse,
  parseAndGetResult: parseAndGetResult
};

function makeParseTree(x) {
  return parseAndGetResult(x);
}

var SnailFish = {
  splittable: splittable,
  split: split,
  pairAtDepthC: pairAtDepthC,
  pairAtDepth: pairAtDepth,
  rightmostNum: rightmostNum,
  rightmostOnLeft: rightmostOnLeft,
  leftmostNum: leftmostNum,
  leftmostOnRight: leftmostOnRight,
  explode: explode,
  reduce: reduce,
  snailAdd: snailAdd,
  total: total,
  magnitude: magnitude,
  part1: part1,
  part2: part2,
  Parser: Parser,
  makeParseTree: makeParseTree
};

function parse$1(data) {
  return Belt_Array.map(Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                    return x.trim();
                  })), makeParseTree);
}

function solvePart1(data) {
  var lines = parse$1(data);
  return part1(lines);
}

function solvePart2(data) {
  var lines = parse$1(data);
  return part2(lines);
}

var P;

var T;

export {
  log ,
  log2 ,
  log3 ,
  P ,
  T ,
  SnailFish ,
  parse$1 as parse,
  solvePart1 ,
  solvePart2 ,
}
/* comma Not a pure module */
