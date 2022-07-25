// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_HashSet from "rescript/lib/es6/belt_HashSet.js";

function eq(param, param$1) {
  return param[0] === param$1[0];
}

function hash(param) {
  return caml_hash_final_mix(caml_hash_mix_string(0, param[0]));
}

var Tuple = {
  eq: eq,
  hash: hash
};

var HashSetTupleCmp = Belt_Id.MakeHashable({
      hash: hash,
      eq: eq
    });

function make(hintSize) {
  return Belt_HashSet.make(hintSize, HashSetTupleCmp);
}

export {
  Tuple ,
  HashSetTupleCmp ,
  make ,
  
}
/* HashSetTupleCmp Not a pure module */