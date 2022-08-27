// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import Debug from "debug";

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
    debug(jsd, message);
  };
  return {
          debug: debug$1
        };
}

export {
  JSD ,
  make ,
}
/* debug Not a pure module */
