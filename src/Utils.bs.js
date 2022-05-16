// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Int64 = require("rescript/lib/js/int64.js");
var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");
var Caml_int64 = require("rescript/lib/js/caml_int64.js");
var Pervasives = require("rescript/lib/js/pervasives.js");
var Belt_MapInt = require("rescript/lib/js/belt_MapInt.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_format = require("rescript/lib/js/caml_format.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Belt_SortArrayInt = require("rescript/lib/js/belt_SortArrayInt.js");
var Belt_HashMapString = require("rescript/lib/js/belt_HashMapString.js");
var Belt_MutableMapInt = require("rescript/lib/js/belt_MutableMapInt.js");
var Belt_MutableMapString = require("rescript/lib/js/belt_MutableMapString.js");
var FP_Utils$AdventOfCode = require("./FP_Utils.bs.js");

function log(prim) {
  console.log(prim);
  
}

function dump_mapString_of(f, m) {
  return Belt_MapString.forEach(m, (function (k, v) {
                var prim = "key:" + k + ", val:" + Curry._1(f, v);
                console.log(prim);
                
              }));
}

function dump_mapString_of_int(param) {
  return dump_mapString_of((function (prim) {
                return String(prim);
              }), param);
}

function dump_mapString_of_string(param) {
  return dump_mapString_of((function (a) {
                return a;
              }), param);
}

function dump_mapInt_of(m, f) {
  return Belt_MapInt.forEach(m, (function (k, v) {
                var prim = "key:" + String(k) + ", val:" + Curry._1(f, v);
                console.log(prim);
                
              }));
}

function dump_mapInt_of_int(__x) {
  return dump_mapInt_of(__x, (function (prim) {
                return String(prim);
              }));
}

function dump_mapInt_of_int64(__x) {
  return dump_mapInt_of(__x, Int64.to_string);
}

function dump_mutableMapInt_of(f, m) {
  return Belt_MutableMapInt.forEach(m, (function (k, v) {
                var prim = "key:" + String(k) + ", val:" + Curry._1(f, v);
                console.log(prim);
                
              }));
}

function dump_mutableMapInt_of_int(param) {
  return dump_mutableMapInt_of((function (prim) {
                return String(prim);
              }), param);
}

function dump_mutableMapInt_of_int64(param) {
  return dump_mutableMapInt_of(Int64.to_string, param);
}

function dump_mutableMapInt_of_int_base2(param) {
  return dump_mutableMapInt_of((function (x) {
                return x.toString(2);
              }), param);
}

function dump_mutableMapString_of(f, m) {
  return Belt_MutableMapString.forEach(m, (function (k, v) {
                var prim = "key:" + k + ", val:" + Curry._1(f, v);
                console.log(prim);
                
              }));
}

function dump_mutableMapString_of_int(param) {
  return dump_mutableMapString_of((function (prim) {
                return String(prim);
              }), param);
}

function dump_mutableMapString_of_int64(param) {
  return dump_mutableMapString_of(Int64.to_string, param);
}

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
  Belt_HashMapString.set(h, k, Belt_Option.mapWithDefault(Belt_HashMapString.get(h, k), Curry._1(f, undefined), (function (x) {
              return Curry._1(f, Caml_option.some(x));
            })));
  return h;
}

exports.log = log;
exports.dump_mapString_of = dump_mapString_of;
exports.dump_mapString_of_int = dump_mapString_of_int;
exports.dump_mapString_of_string = dump_mapString_of_string;
exports.dump_mapInt_of = dump_mapInt_of;
exports.dump_mapInt_of_int = dump_mapInt_of_int;
exports.dump_mapInt_of_int64 = dump_mapInt_of_int64;
exports.dump_mutableMapInt_of = dump_mutableMapInt_of;
exports.dump_mutableMapInt_of_int = dump_mutableMapInt_of_int;
exports.dump_mutableMapInt_of_int64 = dump_mutableMapInt_of_int64;
exports.dump_mutableMapInt_of_int_base2 = dump_mutableMapInt_of_int_base2;
exports.dump_mutableMapString_of = dump_mutableMapString_of;
exports.dump_mutableMapString_of_int = dump_mutableMapString_of_int;
exports.dump_mutableMapString_of_int64 = dump_mutableMapString_of_int64;
exports.base2 = base2;
exports.intFromStringExn = intFromStringExn;
exports.add = add;
exports.sub = sub;
exports.mul = mul;
exports.div = div;
exports.int32ToUint32 = int32ToUint32;
exports.increaseByInt64 = increaseByInt64;
exports.increaseBy1L = increaseBy1L;
exports.increaseBy = increaseBy;
exports.increaseBy1 = increaseBy1;
exports.int64FromBitString = int64FromBitString;
exports.dump_list = dump_list;
exports.splitChars = splitChars;
exports.splitNewline = splitNewline;
exports.splitDoubleNewline = splitDoubleNewline;
exports.sumIntArray = sumIntArray;
exports.join = join;
exports.sumRange = sumRange;
exports.maxIntInArray = maxIntInArray;
exports.minIntInArray = minIntInArray;
exports.flatten = flatten;
exports.maxKeyIntValuePair = maxKeyIntValuePair;
exports.minKeyIntValuePair = minKeyIntValuePair;
exports.maxKeyInt64ValuePair = maxKeyInt64ValuePair;
exports.minKeyInt64ValuePair = minKeyInt64ValuePair;
exports.hashMapStringUpdate = hashMapStringUpdate;
/* No side effect */
