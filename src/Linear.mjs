// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function cmp(param, param$1) {
  var r = Caml_obj.compare(param[0], param$1[0]);
  if (r !== 0) {
    return r;
  } else {
    return Caml_obj.compare(param[1], param$1[1]);
  }
}

function map(param, f) {
  return [
          Curry._1(f, param[0]),
          Curry._1(f, param[1])
        ];
}

var V2 = {
  cmp: cmp,
  map: map
};

function cmp$1(param, param$1) {
  var r = Caml_obj.compare(param[0], param$1[0]);
  if (r !== 0) {
    return r;
  }
  var r$1 = Caml_obj.compare(param[1], param$1[1]);
  if (r$1 !== 0) {
    return r$1;
  } else {
    return Caml_obj.compare(param[2], param$1[2]);
  }
}

function map$1(param, f) {
  return [
          Curry._1(f, param[0]),
          Curry._1(f, param[1]),
          Curry._1(f, param[2])
        ];
}

var V3 = {
  cmp: cmp$1,
  map: map$1
};

export {
  V2 ,
  V3 ,
}
/* No side effect */
