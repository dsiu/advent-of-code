// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var Not_Expected = /* @__PURE__ */Caml_exceptions.create("Tree-AdventOfCode.Not_Expected");

function treeToString(e) {
  if (e.TAG === "Leaf") {
    return e._0.toString();
  }
  var s1 = treeToString(e._0);
  var s2 = treeToString(e._1);
  return "[" + s1 + "," + s2 + "]";
}

function cxtToString(c) {
  if (typeof c !== "object") {
    return "Top";
  }
  if (c.TAG === "L") {
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
  if (t.TAG !== "Leaf") {
    return {
            TAG: "Loc",
            _0: t._0,
            _1: {
              TAG: "L",
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
  if (t.TAG !== "Leaf") {
    return {
            TAG: "Loc",
            _0: t._1,
            _1: {
              TAG: "R",
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
  return {
          TAG: "Loc",
          _0: t,
          _1: "Top"
        };
}

function up(param) {
  var c = param._1;
  var t = param._0;
  if (typeof c === "object") {
    if (c.TAG === "L") {
      return {
              TAG: "Loc",
              _0: {
                TAG: "Pair",
                _0: t,
                _1: c._1
              },
              _1: c._0
            };
    } else {
      return {
              TAG: "Loc",
              _0: {
                TAG: "Pair",
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
    var tmp = l._1;
    if (typeof tmp !== "object") {
      return l;
    }
    if (tmp.TAG === "L") {
      _l = up(l);
      continue ;
    }
    _l = up(l);
    continue ;
  };
}

function modify(param, f) {
  return {
          TAG: "Loc",
          _0: f(param._0),
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
