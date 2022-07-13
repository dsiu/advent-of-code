// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_HashMapString from "rescript/lib/es6/belt_HashMapString.js";
import * as Utils$AdventOfCode from "../../Utils.bs.js";

function log(prim) {
  console.log(prim);
  
}

function update_value_inc_by_1(h, k) {
  return Utils$AdventOfCode.hashMapStringUpdate(h, k, Utils$AdventOfCode.increaseBy1L);
}

function update_value_inc_by_n(h, k, n) {
  return Utils$AdventOfCode.hashMapStringUpdate(h, k, (function (__x) {
                return Utils$AdventOfCode.increaseByInt64(__x, n);
              }));
}

function make(template, rules) {
  var r = Belt_HashMapString.make(40);
  return {
          template: Belt_List.fromArray(template),
          rules: (Belt_Array.forEach(rules, (function (param) {
                    Belt_HashMapString.set(r, param[0], param[1]);
                    
                  })), r)
        };
}

function morph(a, b, rules) {
  var k = a + b;
  return Belt_Option.getExn(Belt_HashMapString.get(rules, k));
}

function iterate_no_tail_opt(param) {
  var inner = function (l, rules) {
    if (!l) {
      return /* [] */0;
    }
    var match = l.tl;
    var last = l.hd;
    if (!match) {
      return {
              hd: last,
              tl: /* [] */0
            };
    }
    var h2 = match.hd;
    return Belt_List.concat({
                hd: last,
                tl: {
                  hd: morph(last, h2, rules),
                  tl: /* [] */0
                }
              }, inner({
                    hd: h2,
                    tl: match.tl
                  }, rules));
  };
  return inner(param.template, param.rules);
}

function iterate_tail_opt(param) {
  var _l = param.template;
  var rules = param.rules;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var l = _l;
    if (!l) {
      return acc;
    }
    var match = l.tl;
    var last = l.hd;
    if (!match) {
      return Belt_List.concat(acc, {
                  hd: last,
                  tl: /* [] */0
                });
    }
    var h2 = match.hd;
    _acc = Belt_List.concat(acc, {
          hd: last,
          tl: {
            hd: morph(last, h2, rules),
            tl: /* [] */0
          }
        });
    _l = {
      hd: h2,
      tl: match.tl
    };
    continue ;
  };
}

function iterateN_tail_opt(param, n) {
  var _t = param.template;
  var r = param.rules;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var t = _t;
    if (n$1 === 0) {
      return t;
    }
    _n = n$1 - 1 | 0;
    _t = iterate_tail_opt({
          template: t,
          rules: r
        });
    continue ;
  };
}

function solve_with_result(t, n) {
  var ret = iterateN_tail_opt(t, n);
  var r = Belt_HashMapString.toArray(Belt_List.reduce(ret, Belt_HashMapString.make(10), (function (acc, k) {
              return Utils$AdventOfCode.hashMapStringUpdate(acc, k, Utils$AdventOfCode.increaseBy1L);
            })));
  var match = Utils$AdventOfCode.maxKeyInt64ValuePair(r);
  var match$1 = Utils$AdventOfCode.minKeyInt64ValuePair(r);
  return Caml_int64.sub(match[1], match$1[1]);
}

function genPairsMap(template) {
  var acc = Belt_HashMapString.make(40);
  var _l = template;
  while(true) {
    var l = _l;
    if (!l) {
      return acc;
    }
    var match = l.tl;
    var last = l.hd;
    if (!match) {
      return Utils$AdventOfCode.hashMapStringUpdate(acc, last, Utils$AdventOfCode.increaseBy1L);
    }
    var h2 = match.hd;
    Utils$AdventOfCode.hashMapStringUpdate(acc, last + h2, Utils$AdventOfCode.increaseBy1L);
    _l = {
      hd: h2,
      tl: match.tl
    };
    continue ;
  };
}

function genNewKeys(k, rules) {
  var match = k.length;
  if (match === 1) {
    return [k];
  }
  if (match !== 2) {
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  var a = k.substring(0, 1);
  var b = k.substring(1, 2);
  var c = morph(a, b, rules);
  return [
          a + c,
          c + b
        ];
}

function iterate(m, rules) {
  var m$p = Belt_HashMapString.make(40);
  Belt_HashMapString.forEach(m, (function (k, v) {
          return Belt_Array.forEach(genNewKeys(k, rules), (function (k$p) {
                        update_value_inc_by_n(m$p, k$p, v);
                        
                      }));
        }));
  return m$p;
}

function iterateN(param, n) {
  var init = genPairsMap(param.template);
  var _m = init;
  var r = param.rules;
  var _n = n;
  while(true) {
    var n$1 = _n;
    var m = _m;
    if (n$1 === 0) {
      return m;
    }
    _n = n$1 - 1 | 0;
    _m = iterate(m, r);
    continue ;
  };
}

function countPolymers(m, template) {
  var r = Belt_HashMapString.make(40);
  Belt_HashMapString.forEach(m, (function (k, v) {
          return Belt_Array.forEach(Utils$AdventOfCode.splitChars(k), (function (c) {
                        update_value_inc_by_n(r, c, v);
                        
                      }));
        }));
  var first_poly = Belt_List.headExn(template);
  Belt_HashMapString.forEach(r, (function (k, v) {
          var v$p = k === first_poly ? Caml_int64.div(Caml_int64.add(v, Caml_int64.one), [
                  0,
                  2
                ]) : Caml_int64.div(v, [
                  0,
                  2
                ]);
          return Belt_HashMapString.set(r, k, v$p);
        }));
  return r;
}

function solve(t, n) {
  var r = iterateN(t, n);
  var c = Belt_HashMapString.toArray(countPolymers(r, t.template));
  var match = Utils$AdventOfCode.maxKeyInt64ValuePair(c);
  var match$1 = Utils$AdventOfCode.minKeyInt64ValuePair(c);
  return Caml_int64.sub(match[1], match$1[1]);
}

function part1(__x) {
  return solve_with_result(__x, 10);
}

function part2(__x) {
  return solve(__x, 40);
}

var Polymer = {
  make: make,
  morph: morph,
  iterate_no_tail_opt: iterate_no_tail_opt,
  iterate_tail_opt: iterate_tail_opt,
  iterateN_tail_opt: iterateN_tail_opt,
  solve_with_result: solve_with_result,
  genPairsMap: genPairsMap,
  genNewKeys: genNewKeys,
  iterate: iterate,
  iterateN: iterateN,
  countPolymers: countPolymers,
  solve: solve,
  part1: part1,
  part2: part2
};

function parse(data) {
  var parsed = Utils$AdventOfCode.splitDoubleNewline(data);
  var template = Belt_Option.getExn(Belt_Array.get(parsed, 0));
  var rules = Belt_Option.getExn(Belt_Array.get(parsed, 1));
  return [
          template.trim().split(""),
          Belt_Array.map(Utils$AdventOfCode.splitNewline(rules), (function (x) {
                  var s = x.trim().split(" -> ");
                  return [
                          Belt_Option.getExn(Belt_Array.get(s, 0)),
                          Belt_Option.getExn(Belt_Array.get(s, 1))
                        ];
                }))
        ];
}

function solvePart1(data) {
  var match = parse(data);
  return solve_with_result(make(match[0], match[1]), 10);
}

function solvePart2(data) {
  var match = parse(data);
  return solve(make(match[0], match[1]), 40);
}

export {
  log ,
  update_value_inc_by_1 ,
  update_value_inc_by_n ,
  Polymer ,
  parse ,
  solvePart1 ,
  solvePart2 ,
  
}
/* No side effect */
