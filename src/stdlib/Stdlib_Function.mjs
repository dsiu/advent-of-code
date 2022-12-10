// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Stdlib_Array$AdventOfCode from "./Stdlib_Array.mjs";

function identity(a) {
  return a;
}

function eq(x, y) {
  return x === y;
}

function composeU(f, g, x) {
  return g(f(x));
}

function compose(f, g, x) {
  return Curry._1(g, Curry._1(f, x));
}

function compose3(f, g, h, x) {
  return Curry._1(h, Curry._1(g, Curry._1(f, x)));
}

function compose4(f, g, h, i, x) {
  return Curry._1(i, Curry._1(h, Curry._1(g, Curry._1(f, x))));
}

function composeN(fs) {
  return Stdlib_Array$AdventOfCode.foldLeft(fs, compose);
}

export {
  identity ,
  eq ,
  composeU ,
  compose ,
  compose3 ,
  compose4 ,
  composeN ,
}
/* No side effect */
