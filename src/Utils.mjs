// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Int64 from "rescript/lib/es6/int64.js";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as Belt_SortArrayInt from "rescript/lib/es6/belt_SortArrayInt.js";
import * as Belt_HashMapString from "rescript/lib/es6/belt_HashMapString.js";
import * as Belt_MutableMapInt from "rescript/lib/es6/belt_MutableMapInt.js";
import * as Belt_MutableMapString from "rescript/lib/es6/belt_MutableMapString.js";
import * as FP_Utils$AdventOfCode from "./FP_Utils.mjs";

function log(prim) {
  console.log(prim);
  
}

function toString(m, f) {
  return Belt_MapString.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k + ", val:" + Curry._1(f, v) + "\n");
              }));
}

function toString$1(m) {
  return toString(m, FP_Utils$AdventOfCode.identity);
}

var $$String = {
  toString: toString$1
};

function toString$2(m) {
  return toString(m, (function (prim) {
                return String(prim);
              }));
}

var Int = {
  toString: toString$2
};

var MapString = {
  toString: toString,
  $$String: $$String,
  Int: Int
};

function toString$3(m, f) {
  return Belt_MapInt.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + String(k) + ", val:" + Curry._1(f, v) + "\n");
              }));
}

function toString$4(m) {
  return toString$3(m, FP_Utils$AdventOfCode.identity);
}

var $$String$2 = {
  toString: toString$4
};

function toString$5(m) {
  return toString$3(m, (function (prim) {
                return String(prim);
              }));
}

var Int$1 = {
  toString: toString$5
};

function toString$6(m) {
  return toString$3(m, Int64.to_string);
}

var Int64$1 = {
  toString: toString$6
};

var MapInt = {
  toString: toString$3,
  $$String: $$String$2,
  Int: Int$1,
  Int64: Int64$1
};

function toString$7(m, f) {
  return Belt_MutableMapInt.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + String(k) + ", val:" + Curry._1(f, v));
              }));
}

function toString$8(m) {
  return toString$7(m, (function (prim) {
                return String(prim);
              }));
}

var Int$2 = {
  toString: toString$8
};

function toString$9(m) {
  return toString$7(m, Int64.to_string);
}

var Int64$2 = {
  toString: toString$9
};

function toString$10(m) {
  return toString$7(m, (function (x) {
                return x.toString(2);
              }));
}

var IntBase2 = {
  toString: toString$10
};

var MutableMapInt = {
  toString: toString$7,
  Int: Int$2,
  Int64: Int64$2,
  IntBase2: IntBase2
};

function toString$11(m, f) {
  return Belt_MutableMapString.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k + ", val:" + Curry._1(f, v));
              }));
}

function toString$12(m) {
  return toString$11(m, (function (prim) {
                return String(prim);
              }));
}

var Int$3 = {
  toString: toString$12
};

function toString$13(m) {
  return toString$11(m, Int64.to_string);
}

var Int64$3 = {
  toString: toString$13
};

var MutableMapString = {
  toString: toString$11,
  Int: Int$3,
  Int64: Int64$3
};

function base2(__x) {
  return __x.toString(2);
}

function intFromStringExn(param) {
  return FP_Utils$AdventOfCode.compose(Belt_Int.fromString, Belt_Option.getExn, param);
}

function add(x, y) {
  return x + y | 0;
}

function sub(x, y) {
  return x - y | 0;
}

function mul(x, y) {
  return Math.imul(x, y);
}

var div = Caml_int32.div;

function int32ToUint32(x) {
  return new Uint32Array([x])[0];
}

function increaseByInt64(v, n) {
  return Belt_Option.mapWithDefault(v, n, (function (x) {
                return Caml_int64.add(x, n);
              }));
}

function increaseBy1L(__x) {
  return increaseByInt64(__x, Caml_int64.one);
}

function increaseBy(v, n) {
  return Belt_Option.mapWithDefault(v, n, (function (x) {
                return x + n | 0;
              }));
}

function increaseBy1(__x) {
  return increaseBy(__x, 1);
}

function int64FromBitString(str) {
  return Caml_format.caml_int64_of_string("0b" + str);
}

function dump_list(__x) {
  return Belt_List.forEach(__x, log);
}

function splitChars(__x) {
  return __x.split("");
}

function splitNewline(__x) {
  return __x.split("\n");
}

function splitDoubleNewline(__x) {
  return __x.split("\n\n");
}

function sumIntArray(__x) {
  return Belt_Array.reduce(__x, 0, add);
}

function join(__x) {
  return __x.join("");
}

function sumRange(xs, offset, len) {
  var elems = Belt_Array.slice(xs, offset, len);
  var total = {
    contents: 0
  };
  Belt_Array.forEach(elems, (function (x) {
          total.contents = total.contents + x | 0;
          
        }));
  return total.contents;
}

function maxIntInArray(xs) {
  var sorted = Belt_SortArrayInt.stableSort(xs);
  return Belt_Array.getExn(sorted, sorted.length - 1 | 0);
}

function minIntInArray(xs) {
  var sorted = Belt_SortArrayInt.stableSort(xs);
  return Belt_Array.getExn(sorted, 0);
}

function flatten(xs) {
  return Belt_Array.reduce(xs, [], Belt_Array.concat);
}

function maxKeyIntValuePair(__x) {
  return Belt_Array.reduce(__x, [
              "",
              0
            ], (function (acc, param) {
                var v = param[1];
                if (v > acc[1]) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function minKeyIntValuePair(__x) {
  return Belt_Array.reduce(__x, [
              "",
              Pervasives.max_int
            ], (function (acc, param) {
                var v = param[1];
                if (v < acc[1]) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function maxKeyInt64ValuePair(__x) {
  return Belt_Array.reduce(__x, [
              "",
              Caml_int64.zero
            ], (function (acc, param) {
                var v = param[1];
                if (Int64.compare(v, acc[1]) > 0) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function minKeyInt64ValuePair(__x) {
  return Belt_Array.reduce(__x, [
              "",
              Int64.max_int
            ], (function (acc, param) {
                var v = param[1];
                if (Int64.compare(v, acc[1]) < 0) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function hashMapStringUpdate(h, k, f) {
  Belt_HashMapString.set(h, k, Belt_Option.mapWithDefaultU(Belt_HashMapString.get(h, k), Curry._1(f, undefined), (function (x) {
              return Curry._1(f, Caml_option.some(x));
            })));
  return h;
}

var identity = FP_Utils$AdventOfCode.identity;

export {
  log ,
  identity ,
  MapString ,
  MapInt ,
  MutableMapInt ,
  MutableMapString ,
  base2 ,
  intFromStringExn ,
  add ,
  sub ,
  mul ,
  div ,
  int32ToUint32 ,
  increaseByInt64 ,
  increaseBy1L ,
  increaseBy ,
  increaseBy1 ,
  int64FromBitString ,
  dump_list ,
  splitChars ,
  splitNewline ,
  splitDoubleNewline ,
  sumIntArray ,
  join ,
  sumRange ,
  maxIntInArray ,
  minIntInArray ,
  flatten ,
  maxKeyIntValuePair ,
  minKeyIntValuePair ,
  maxKeyInt64ValuePair ,
  minKeyInt64ValuePair ,
  hashMapStringUpdate ,
  
}
/* No side effect */
