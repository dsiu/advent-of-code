// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function log3(prim0, prim1, prim2) {
  console.log(prim0, prim1, prim2);
  
}

function dumpTree(e) {
  if (e.TAG === /* Leaf */0) {
    return "Leaf:" + String(e._0);
  }
  var s1 = dumpTree(e._0);
  var s2 = dumpTree(e._1);
  return "[" + s1 + ", " + s2 + "]";
}

function dumpCxt(c) {
  if (typeof c === "number") {
    return "Top";
  }
  if (c.TAG === /* L */0) {
    var t_str = dumpTree(c._1);
    var c_str = dumpCxt(c._0);
    return "L(" + c_str + ", " + t_str + ")";
  }
  var t_str$1 = dumpTree(c._0);
  var c_str$1 = dumpCxt(c._1);
  return "R(" + c_str$1 + ", " + t_str$1 + ")";
}

function dumpLoc(param) {
  var t_str = dumpTree(param[0]);
  var c_str = dumpCxt(param[1]);
  return "Loc(cxt=" + c_str + ", tree=" + t_str + ")";
}

function left(l) {
  var t = l[0];
  if (t.TAG === /* Leaf */0) {
    return l;
  } else {
    return [
            t._0,
            {
              TAG: /* L */0,
              _0: l[1],
              _1: t._1
            }
          ];
  }
}

function right(l) {
  var t = l[0];
  if (t.TAG === /* Leaf */0) {
    return l;
  } else {
    return [
            t._1,
            {
              TAG: /* R */1,
              _0: t._0,
              _1: l[1]
            }
          ];
  }
}

function top(t) {
  return [
          t,
          /* Top */0
        ];
}

function up(l) {
  var c = l[1];
  var t = l[0];
  if (typeof c === "number") {
    return l;
  } else if (c.TAG === /* L */0) {
    return [
            {
              TAG: /* Pair */1,
              _0: t,
              _1: c._1
            },
            c._0
          ];
  } else {
    return [
            {
              TAG: /* Pair */1,
              _0: c._0,
              _1: t
            },
            c._1
          ];
  }
}

function upmost(_l) {
  while(true) {
    var l = _l;
    if (typeof l[1] === "number") {
      return l;
    }
    _l = up(l);
    continue ;
  };
}

function modify(param, f) {
  return [
          Curry._1(f, param[0]),
          param[1]
        ];
}

var t_0 = {
  TAG: /* Pair */1,
  _0: {
    TAG: /* Leaf */0,
    _0: 1
  },
  _1: {
    TAG: /* Leaf */0,
    _0: 2
  }
};

var t_1 = {
  TAG: /* Pair */1,
  _0: {
    TAG: /* Leaf */0,
    _0: 3
  },
  _1: {
    TAG: /* Leaf */0,
    _0: 4
  }
};

var t = {
  TAG: /* Pair */1,
  _0: t_0,
  _1: t_1
};

var prim1 = dumpLoc(right(left([
              t,
              /* Top */0
            ])));

console.log("trying to reach Leaf 2\n", prim1);

var prim1$1 = dumpLoc(modify(right(left([
                  t,
                  /* Top */0
                ])), (function (param) {
            return {
                    TAG: /* Leaf */0,
                    _0: 0
                  };
          })));

console.log("modifying Leaf 2\n", prim1$1);

var prim1$2 = dumpTree(upmost(modify(right(left([
                        t,
                        /* Top */0
                      ])), (function (param) {
                  return {
                          TAG: /* Leaf */0,
                          _0: 0
                        };
                })))[0]);

console.log("upmost\n", prim1$2);

var Tree = {
  dumpTree: dumpTree,
  dumpCxt: dumpCxt,
  dumpLoc: dumpLoc,
  left: left,
  right: right,
  top: top,
  up: up,
  upmost: upmost,
  modify: modify
};

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
                TAG: /* Leaf */0,
                _0: Belt_Option.getExn(Belt_Int.fromString(x))
              };
      };
      var pairOrNumber = Res_parser.choice([
            p,
            Res_parser.map(digits, makeIntElem)
          ]);
      return Res_parser.map(betweenBraces(Res_parser.andThen(Res_parser.keepLeft(pairOrNumber, comma), pairOrNumber)), (function (param) {
                    return {
                            TAG: /* Pair */1,
                            _0: param[0],
                            _1: param[1]
                          };
                  }));
    });

function parse(s) {
  return Res_parser.run(pair, s);
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
  parse: parse
};

var SnailFish = {
  Parser: Parser
};

function solvePart1(data) {
  var l = Res_parser.run(pair, data);
  var prim1 = Belt_Result.isOk(l);
  console.log("parse result:", prim1);
  var p = Belt_Result.getExn(l)[0];
  if (l.TAG === /* Ok */0) {
    var prim1$1 = dumpTree(p);
    console.log("Parsed as: ", prim1$1);
    var prim1$2 = Belt_Result.getExn(l)[1];
    console.log("Parse state:", prim1$2);
    console.log("\n");
  } else {
    console.log(l._0);
    console.log("\n");
  }
  return 1;
}

function solvePart2(data) {
  return 2;
}

var P;

var Rjs;

export {
  log ,
  log2 ,
  log3 ,
  P ,
  Rjs ,
  Tree ,
  SnailFish ,
  solvePart1 ,
  solvePart2 ,
  
}
/* prim1 Not a pure module */
