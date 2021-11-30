// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Int64 = require("rescript/lib/js/int64.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_MapInt = require("rescript/lib/js/belt_MapInt.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Belt_SortArrayInt = require("rescript/lib/js/belt_SortArrayInt.js");
var Belt_MutableMapInt = require("rescript/lib/js/belt_MutableMapInt.js");

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

function sum(a, x) {
  return a + x | 0;
}

function sumIntArray(__x) {
  return Belt_Array.reduce(__x, 0, sum);
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

function int32ToUint32(x) {
  return new Uint32Array([x])[0];
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
exports.dump_list = dump_list;
exports.splitChars = splitChars;
exports.splitNewline = splitNewline;
exports.splitDoubleNewline = splitDoubleNewline;
exports.sum = sum;
exports.sumIntArray = sumIntArray;
exports.join = join;
exports.sumRange = sumRange;
exports.maxIntInArray = maxIntInArray;
exports.minIntInArray = minIntInArray;
exports.int32ToUint32 = int32ToUint32;
/* No side effect */
