// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_math = require("rescript/lib/js/js_math.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function mulInv(a, b) {
  var x0 = 0;
  var x1 = 1;
  if (b === 1) {
    return 1;
  }
  var aa = a;
  var bb = b;
  while(aa > 1) {
    var q = Js_math.floor_int(aa / bb);
    var c = Caml_int32.mod_(aa, bb);
    aa = bb;
    bb = c;
    var tmp = x0;
    x0 = x1 - Math.imul(q, x0) | 0;
    x1 = tmp;
  };
  if (x1 < 0) {
    x1 = x1 + b | 0;
  }
  return x1;
}

function crt(rem, num) {
  var sum = 0;
  var prod = Belt_Array.reduce(num, 1, (function (a, c) {
          return Math.imul(a, c);
        }));
  for(var i = 0 ,i_finish = num.length; i < i_finish; ++i){
    var ni = Belt_Option.getExn(Belt_Array.get(num, i));
    var ri = Belt_Option.getExn(Belt_Array.get(rem, i));
    var p = Js_math.floor_int(prod / ni);
    sum = sum + Math.imul(Math.imul(ri, p), mulInv(p, ni)) | 0;
  }
  return Caml_int32.mod_(sum, prod);
}

var big_zero = BigInt(0);

var big_one = BigInt(1);

function mulInvBigInt(a, b) {
  var x0 = big_zero;
  var x1 = big_one;
  if (b === big_one) {
    return big_one;
  }
  var aa = a;
  var bb = b;
  while((aa > big_one)) {
    var q = aa / bb;
    var c = aa % bb;
    aa = bb;
    bb = c;
    var tmp = x0;
    x0 = x1 - q * x0;
    x1 = tmp;
  };
  if ((x1 < big_zero)) {
    x1 = x1 + b;
  }
  return x1;
}

function crtBigInt(rem, num) {
  var sum = big_zero;
  var prod = Belt_Array.reduce(num, big_one, (function (a, c) {
          return a * c;
        }));
  for(var i = 0 ,i_finish = num.length; i < i_finish; ++i){
    var ni = Belt_Option.getExn(num[i]);
    var ri = Belt_Option.getExn(rem[i]);
    var p = prod / ni;
    sum = sum + ri * p * mulInvBigInt(p, ni);
  }
  return sum % prod;
}

exports.mulInv = mulInv;
exports.crt = crt;
exports.big_zero = big_zero;
exports.big_one = big_one;
exports.mulInvBigInt = mulInvBigInt;
exports.crtBigInt = crtBigInt;
/* big_zero Not a pure module */
