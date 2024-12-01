// Generated by ReScript, PLEASE EDIT WITH CARE

import Debug from "debug";

function debug(t, s) {
  return t(s);
}

function debug2(t2, s, o) {
  return t2(s, o);
}

let JSD = {
  debug: debug,
  debug2: debug2
};

function make(moduleName) {
  let jsd = Debug(moduleName);
  let debug = message => jsd(message);
  return {
    debug: debug
  };
}

export {
  JSD,
  make,
}
/* debug Not a pure module */
