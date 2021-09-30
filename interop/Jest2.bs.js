// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var $$Promise = require("@ryyppy/rescript-promise/src/Promise.bs.js");

function testEach(title, data, f) {
  return test.each(data)(title + "(%#) %p", f);
}

function testEach2(title, data, f) {
  return test.each(data)(title + "(%#) %p %p", f);
}

function testEach3(title, data, f) {
  return test.each(data)(title + "(%#) %p %p %p", f);
}

function testEach4(title, data, f) {
  return test.each(data)(title + "(%#) %p %p %p %p", f);
}

function awaitThen(pa, done, f) {
  $$Promise.$$catch(pa.then(function (a) {
            Curry._1(f, a);
            return Curry._1(done, undefined);
          }), done);
  
}

exports.testEach = testEach;
exports.testEach2 = testEach2;
exports.testEach3 = testEach3;
exports.testEach4 = testEach4;
exports.awaitThen = awaitThen;
/* No side effect */
