// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Int64 from "rescript/lib/es6/int64.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_SortArray from "rescript/lib/es6/belt_SortArray.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Stack_Array$AdventOfCode from "../../Stack_Array.mjs";

function log(prim) {
  console.log(prim);
}

var ParseError = /* @__PURE__ */Caml_exceptions.create("AOC2021_Day10-AdventOfCode.ParseError");

var NotSupported = /* @__PURE__ */Caml_exceptions.create("AOC2021_Day10-AdventOfCode.NotSupported");

function isOpenBracket(token) {
  if (token === "{" || token === "[" || token === "<") {
    return true;
  } else {
    return token === "(";
  }
}

function isCloseBracket(token) {
  if (token === "}" || token === "]" || token === ">") {
    return true;
  } else {
    return token === ")";
  }
}

function matches(left, right) {
  if (left === "(") {
    return right === ")";
  } else if (left === "<") {
    return right === ">";
  } else if (left === "[") {
    return right === "]";
  } else if (left === "{") {
    return right === "}";
  } else {
    return false;
  }
}

function make(c) {
  switch (c) {
    case "(" :
        return "(";
    case ")" :
        return ")";
    case "<" :
        return "<";
    case ">" :
        return ">";
    case "[" :
        return "[";
    case "]" :
        return "]";
    case "{" :
        return "{";
    case "}" :
        return "}";
    default:
      throw {
            RE_EXN_ID: ParseError,
            _1: c + " is not supported",
            Error: new Error()
          };
  }
}

function toString(x) {
  return x;
}

var Token = {
  isOpenBracket: isOpenBracket,
  isCloseBracket: isCloseBracket,
  matches: matches,
  make: make,
  toString: toString
};

function tokenAtToString(param) {
  return "'" + param._0 + "':" + String(param._1);
}

function tokenize(xs) {
  return Belt_Array.mapWithIndex(xs, (function (i, x) {
                return {
                        TAG: "TokenAt",
                        _0: make(x),
                        _1: i
                      };
              }));
}

function map(t, f) {
  if (typeof t !== "object") {
    return "Empty";
  }
  if (t.TAG !== "Node") {
    return {
            TAG: "NodeList",
            _0: Belt_List.map(t._0, (function (__x) {
                    return map(__x, f);
                  }))
          };
  }
  var tl = t.tl;
  var b1 = t.l;
  if (typeof tl !== "object") {
    return {
            TAG: "Node",
            l: Curry._1(f, b1),
            tl: "Empty",
            r: Curry._1(f, t.r)
          };
  }
  if (tl.TAG !== "Node") {
    return {
            TAG: "Node",
            l: Curry._1(f, b1),
            tl: map({
                  TAG: "NodeList",
                  _0: tl._0
                }, f),
            r: Curry._1(f, t.r)
          };
  }
  throw {
        RE_EXN_ID: NotSupported,
        _1: "nested nodes",
        Error: new Error()
      };
}

function toString$1(t) {
  if (typeof t !== "object") {
    return "Empty";
  }
  if (t.TAG !== "Node") {
    return "NodeList:{ " + Belt_List.toArray(Belt_List.map(t._0, toString$1)).join(", ") + " }";
  }
  var tl = t.tl;
  var b1 = t.l;
  if (typeof tl !== "object") {
    return "Node(" + tokenAtToString(b1) + ", tl: Empty, " + tokenAtToString(t.r) + "})";
  }
  tl.TAG === "Node";
  return "Node(" + tokenAtToString(b1) + ", tl: " + toString$1(tl) + ", " + tokenAtToString(t.r) + "})";
}

function makeNode(l, r) {
  return {
          TAG: "Node",
          l: l,
          tl: "Empty",
          r: r
        };
}

function makeNodeFromStr(l, li, r, ri) {
  return {
          TAG: "Node",
          l: {
            TAG: "TokenAt",
            _0: make(l),
            _1: li
          },
          tl: "Empty",
          r: {
            TAG: "TokenAt",
            _0: make(r),
            _1: ri
          }
        };
}

function add(a, b) {
  if (typeof a !== "object") {
    return b;
  }
  if (a.TAG === "Node") {
    var tl = a.tl;
    var l = a.l;
    if (typeof tl !== "object") {
      if (typeof b !== "object") {
        return a;
      } else if (b.TAG === "Node") {
        return {
                TAG: "NodeList",
                _0: {
                  hd: a,
                  tl: {
                    hd: b,
                    tl: /* [] */0
                  }
                }
              };
      } else {
        return {
                TAG: "Node",
                l: l,
                tl: b,
                r: a.r
              };
      }
    }
    if (tl.TAG !== "Node") {
      return {
              TAG: "Node",
              l: l,
              tl: add({
                    TAG: "NodeList",
                    _0: tl._0
                  }, b),
              r: a.r
            };
    }
    throw {
          RE_EXN_ID: NotSupported,
          _1: "nested nodes",
          Error: new Error()
        };
  } else {
    var tl$1 = a._0;
    if (typeof b !== "object") {
      return a;
    }
    if (b.TAG !== "Node") {
      return {
              TAG: "NodeList",
              _0: Belt_List.concat(tl$1, b._0)
            };
    }
    var btl = b.tl;
    var l$1 = b.l;
    if (typeof btl !== "object") {
      return {
              TAG: "Node",
              l: l$1,
              tl: {
                TAG: "NodeList",
                _0: tl$1
              },
              r: b.r
            };
    }
    if (btl.TAG !== "Node") {
      return {
              TAG: "Node",
              l: l$1,
              tl: {
                TAG: "NodeList",
                _0: Belt_List.concat(tl$1, btl._0)
              },
              r: b.r
            };
    }
    throw {
          RE_EXN_ID: NotSupported,
          _1: "not supported",
          Error: new Error()
        };
  }
}

function makeParseTree(xs) {
  var match = Belt_List.fromArray(tokenize(xs));
  var match$1;
  if (match) {
    match$1 = [
      match.hd,
      match.tl
    ];
  } else {
    throw {
          RE_EXN_ID: ParseError,
          _1: "empty input",
          Error: new Error()
        };
  }
  var _inputs = match$1[1];
  var _tree = "Empty";
  var _stack = [match$1[0]];
  while(true) {
    var stack = _stack;
    var tree = _tree;
    var inputs = _inputs;
    if (!inputs) {
      return [
              tree,
              stack
            ];
    }
    var rest = inputs.tl;
    var $$this = inputs.hd;
    console.log("processing", $$this);
    console.log("  tree", toString$1(tree));
    console.log("  stack", stack);
    var last = Stack_Array$AdventOfCode.peek(stack);
    var match$2 = isCloseBracket($$this._0);
    if (match$2) {
      if (last !== undefined) {
        var match$3 = Stack_Array$AdventOfCode.pop(stack);
        _stack = match$3[1];
        _tree = add(tree, {
              TAG: "Node",
              l: last,
              tl: "Empty",
              r: $$this
            });
        _inputs = rest;
        continue ;
      }
      _stack = Stack_Array$AdventOfCode.push(stack, $$this);
      _inputs = rest;
      continue ;
    }
    _stack = Stack_Array$AdventOfCode.push(stack, $$this);
    _inputs = rest;
    continue ;
  };
}

var ParseTree = {
  map: map,
  toString: toString$1,
  makeNode: makeNode,
  makeNodeFromStr: makeNodeFromStr,
  add: add,
  makeParseTree: makeParseTree
};

function $$process(xs) {
  var match = Belt_List.fromArray(tokenize(xs));
  var match$1;
  if (match) {
    match$1 = [
      match.hd,
      match.tl
    ];
  } else {
    throw {
          RE_EXN_ID: ParseError,
          _1: "empty input",
          Error: new Error()
        };
  }
  var _inputs = match$1[1];
  var _stack = [match$1[0]];
  while(true) {
    var stack = _stack;
    var inputs = _inputs;
    if (!inputs) {
      return {
              TAG: "Incomplete",
              _0: stack
            };
    }
    var rest = inputs.tl;
    var $$this = inputs.hd;
    var last = Stack_Array$AdventOfCode.peek(stack);
    var this_token = $$this._0;
    var match$2 = isCloseBracket(this_token);
    if (match$2) {
      if (last !== undefined) {
        if (!matches(last._0, this_token)) {
          return {
                  TAG: "Corrupted",
                  _0: $$this
                };
        }
        var match$3 = Stack_Array$AdventOfCode.pop(stack);
        _stack = match$3[1];
        _inputs = rest;
        continue ;
      }
      _stack = Stack_Array$AdventOfCode.push(stack, $$this);
      _inputs = rest;
      continue ;
    }
    _stack = Stack_Array$AdventOfCode.push(stack, $$this);
    _inputs = rest;
    continue ;
  };
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (param) {
                return Stdlib__Function.compose((function (prim) {
                              return prim.trim();
                            }), Utils$AdventOfCode.splitChars, param);
              }));
}

function examples(param) {
  var parent = makeNodeFromStr("(", 1, ")", 2);
  var t = add(add(parent, makeNodeFromStr("[", 3, "]", 4)), makeNodeFromStr("<", 5, ">", 6));
  map(t, (function (x) {
          console.log(x);
          return x;
        }));
  map(t, (function (param) {
          var i = param._1;
          var a = param._0;
          if (a === "}" || a === "]" || a === ">" || a === ")") {
            console.log(i, "Close");
          } else {
            console.log(i, "Open");
          }
          return {
                  TAG: "TokenAt",
                  _0: a,
                  _1: i
                };
        }));
}

function getCorruptedScore(param) {
  var t = param._0;
  if (t === ")") {
    return 3;
  }
  if (t === ">") {
    return 25137;
  }
  if (t === "]") {
    return 57;
  }
  if (t === "}") {
    return 1197;
  }
  throw {
        RE_EXN_ID: NotSupported,
        _1: "not supported",
        Error: new Error()
      };
}

function getIncompleteScore(param) {
  var t = param._0;
  if (t === "(") {
    return 1;
  }
  if (t === "<") {
    return 4;
  }
  if (t === "[") {
    return 2;
  }
  if (t === "{") {
    return 3;
  }
  throw {
        RE_EXN_ID: NotSupported,
        _1: "not supported",
        Error: new Error()
      };
}

function solvePart1(data) {
  var corruptedOnly = function (r) {
    if (r.TAG === "Corrupted") {
      return r._0;
    }
    
  };
  return Belt_Array.reduce(Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(parse(data), $$process), corruptedOnly), getCorruptedScore), 0, Utils$AdventOfCode.add);
}

function solvePart2(data) {
  var incompleteOnly = function (r) {
    if (r.TAG === "Corrupted") {
      return ;
    } else {
      return r._0;
    }
  };
  var xs = Belt_SortArray.stableSortBy(Belt_Array.map(Belt_Array.keepMap(Belt_Array.map(parse(data), $$process), incompleteOnly), (function (__x) {
              return Belt_Array.reduce(__x, Int64.zero, (function (a, x) {
                            return Caml_int64.add(Caml_int64.mul(a, [
                                            0,
                                            5
                                          ]), Caml_int64.of_int32(getIncompleteScore(x)));
                          }));
            })), Int64.compare);
  var len = xs.length;
  return Int64.to_string(Belt_Option.getExn(Belt_Array.get(xs, len / 2 | 0)));
}

var Stack;

export {
  log ,
  Stack ,
  ParseError ,
  NotSupported ,
  Token ,
  tokenAtToString ,
  tokenize ,
  ParseTree ,
  $$process ,
  parse ,
  examples ,
  getCorruptedScore ,
  getIncompleteScore ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
