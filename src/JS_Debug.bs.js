// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Debug = require("debug").default;

function debug(t, s) {
  return Curry._1(t, s);
}

function debug2(t2, s, o) {
  return Curry._2(t2, s, o);
}

var JSD = {
  debug: debug,
  debug2: debug2
};

function make(moduleName) {
  var jsd = Debug(moduleName);
  var debug$1 = function (message) {
    return debug(jsd, message);
  };
  return {
          debug: debug$1
        };
}

exports.JSD = JSD;
exports.make = make;
/* debug Not a pure module */
