// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_MapInt = require("rescript/lib/js/belt_MapInt.js");
var Belt_MutableMapInt = require("rescript/lib/js/belt_MutableMapInt.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");

function log(prim) {
  console.log(prim);
  
}

function inc(t, k) {
  return Belt_MapInt.set(t, k, Belt_MapInt.getWithDefault(t, k, 0) + 1 | 0);
}

function mostCommon(t) {
  var one_count = Belt_MapInt.get(t, 1);
  var zero_count = Belt_MapInt.get(t, 0);
  if (Caml_obj.caml_equal(one_count, zero_count) || Caml_obj.caml_greaterthan(one_count, zero_count)) {
    return 1;
  } else {
    return 0;
  }
}

function leastCommon(t) {
  var one_count = Belt_MapInt.get(t, 1);
  var zero_count = Belt_MapInt.get(t, 0);
  if (Caml_obj.caml_equal(one_count, zero_count) || !Caml_obj.caml_lessthan(one_count, zero_count)) {
    return 0;
  } else {
    return 1;
  }
}

function make(param) {
  return Belt_Array.reduce([
              0,
              1
            ], undefined, (function (a, x) {
                return Belt_MapInt.set(a, x, 0);
              }));
}

var CountContainer = {
  set: Belt_MapInt.set,
  get: Belt_MapInt.get,
  getWithDefault: Belt_MapInt.getWithDefault,
  reduce: Belt_MapInt.reduce,
  inc: inc,
  mostCommon: mostCommon,
  leastCommon: leastCommon,
  make: make
};

function getSafe(m, k) {
  return Belt_MutableMapInt.getWithDefault(m, k, make(undefined));
}

var Total = {
  set: Belt_MutableMapInt.set,
  get: Belt_MutableMapInt.get,
  getSafe: getSafe,
  forEach: Belt_MutableMapInt.forEach,
  map: Belt_MutableMapInt.map,
  make: Belt_MutableMapInt.make
};

function logAndCont(x) {
  console.log("binStringToInt", x);
  return x;
}

function bitArrayToInt(x) {
  return parseInt(Belt_Array.map(x, (function (prim) {
                      return String(prim);
                    })).join(""), 2);
}

function calTotal(xs) {
  return Belt_Array.reduce(xs, Belt_MutableMapInt.make(undefined), (function (a, bits) {
                Belt_Array.forEachWithIndex(bits, (function (idx, bit_val) {
                        var orig_total = getSafe(a, idx);
                        return Belt_MutableMapInt.set(a, idx, inc(orig_total, bit_val));
                      }));
                return a;
              }));
}

function calGamma(xs) {
  return Belt_MutableMapInt.valuesToArray(Belt_MutableMapInt.map(calTotal(xs), mostCommon));
}

function calEpsilon(xs) {
  return Belt_MutableMapInt.valuesToArray(Belt_MutableMapInt.map(calTotal(xs), leastCommon));
}

function part1(xs) {
  var gamma = calGamma(xs);
  var epsilon = calEpsilon(xs);
  return Math.imul(bitArrayToInt(gamma), bitArrayToInt(epsilon));
}

function findRating(xs, func) {
  var _bit_pos = 0;
  var _inputs = xs;
  while(true) {
    var inputs = _inputs;
    var bit_pos = _bit_pos;
    var len = inputs.length;
    if (len === 1) {
      return inputs[0];
    }
    if (len !== 0) {
      var filter = Curry._1(func, inputs);
      var criteria = Belt_Array.getExn(filter, bit_pos);
      var next_inputs = Belt_Array.keep(inputs, (function(bit_pos,criteria){
          return function (x) {
            return Caml_obj.caml_equal(Belt_Array.getExn(x, bit_pos), criteria);
          }
          }(bit_pos,criteria)));
      _inputs = next_inputs;
      _bit_pos = bit_pos + 1 | 0;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
}

function calOxygen(__x) {
  return findRating(__x, calGamma);
}

function calCO2(__x) {
  return findRating(__x, calEpsilon);
}

function part2(xs) {
  var oxygen = findRating(xs, calGamma);
  var co2 = findRating(xs, calEpsilon);
  return Math.imul(bitArrayToInt(oxygen), bitArrayToInt(co2));
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (line) {
                return Belt_Array.map(Utils$AdventOfCode.splitChars(line.trim()), Utils$AdventOfCode.intFromStringExn);
              }));
}

function solvePart1(data) {
  return part1(parse(data));
}

function solvePart2(data) {
  return part2(parse(data));
}

var toBinaryString = Belt_MutableMapInt.valuesToArray;

exports.log = log;
exports.CountContainer = CountContainer;
exports.Total = Total;
exports.toBinaryString = toBinaryString;
exports.logAndCont = logAndCont;
exports.bitArrayToInt = bitArrayToInt;
exports.calTotal = calTotal;
exports.calGamma = calGamma;
exports.calEpsilon = calEpsilon;
exports.part1 = part1;
exports.findRating = findRating;
exports.calOxygen = calOxygen;
exports.calCO2 = calCO2;
exports.part2 = part2;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
