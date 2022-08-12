// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var Not_Expected = /* @__PURE__ */Caml_exceptions.create("Tree-AdventOfCode.Not_Expected");

function treeToString(e) {
  if (e.TAG === /* Leaf */0) {
    return String(e._0);
  }
  var s1 = treeToString(e._0);
  var s2 = treeToString(e._1);
  return "[" + s1 + "," + s2 + "]";
}

function cxtToString(c) {
  if (typeof c === "number") {
    return "Top";
  }
  if (c.TAG === /* L */0) {
    var t_str = treeToString(c._1);
    var c_str = cxtToString(c._0);
    return "L(" + c_str + "," + t_str + ")";
  }
  var t_str$1 = treeToString(c._0);
  var c_str$1 = cxtToString(c._1);
  return "R(" + t_str$1 + "," + c_str$1 + ")";
}

function locToString(param) {
  var t_str = treeToString(param._0);
  var c_str = cxtToString(param._1);
  return "Loc[ cxt = " + c_str + ", tree = " + t_str + " ]";
}

function left(param) {
  var t = param._0;
  if (t.TAG !== /* Leaf */0) {
    return /* Loc */{
            _0: t._0,
            _1: {
              TAG: /* L */0,
              _0: param._1,
              _1: t._1
            }
          };
  }
  throw {
        RE_EXN_ID: Not_Expected,
        _1: "left: not a Pair",
        Error: new Error()
      };
}

function right(param) {
  var t = param._0;
  if (t.TAG !== /* Leaf */0) {
    return /* Loc */{
            _0: t._1,
            _1: {
              TAG: /* R */1,
              _0: t._0,
              _1: param._1
            }
          };
  }
  throw {
        RE_EXN_ID: Not_Expected,
        _1: "right: not a Pair",
        Error: new Error()
      };
}

function top(t) {
  return /* Loc */{
          _0: t,
          _1: /* Top */0
        };
}

function up(param) {
  var c = param._1;
  var t = param._0;
  if (typeof c !== "number") {
    if (c.TAG === /* L */0) {
      return /* Loc */{
              _0: {
                TAG: /* Pair */1,
                _0: t,
                _1: c._1
              },
              _1: c._0
            };
    } else {
      return /* Loc */{
              _0: {
                TAG: /* Pair */1,
                _0: c._0,
                _1: t
              },
              _1: c._1
            };
    }
  }
  throw {
        RE_EXN_ID: Not_Expected,
        _1: "up: up of Top",
        Error: new Error()
      };
}

function upmost(_l) {
  while(true) {
    var l = _l;
    if (typeof l._1 === "number") {
      return l;
    }
    _l = up(l);
    continue ;
  };
}

function modify(param, f) {
  return /* Loc */{
          _0: Curry._1(f, param._0),
          _1: param._1
        };
}

export {
  Not_Expected ,
  treeToString ,
  cxtToString ,
  locToString ,
  left ,
  right ,
  top ,
  up ,
  upmost ,
  modify ,
  
}
/* No side effect */
