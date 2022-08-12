// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";

function flatMapList(xs, f) {
  return Belt_List.reduce(Belt_List.map(xs, f), /* [] */0, Belt_List.concat);
}

function flatMapArray(xs, f) {
  return Belt_Array.reduce(Belt_Array.map(xs, f), [], Belt_Array.concat);
}

function foldlArray(xs, f) {
  var init = Belt_Array.getExn(xs, 0);
  var rest = Belt_Array.sliceToEnd(xs, 1);
  return Belt_Array.reduce(rest, init, f);
}

function foldrArray(xs, f) {
  var end = xs.length - 1 | 0;
  var init = Belt_Array.getExn(xs, end);
  var rest = Belt_Array.slice(xs, 0, end);
  return Belt_Array.reduceReverse(rest, init, f);
}

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
  return Belt_Array.reduce(Belt_Array.sliceToEnd(fs, 1), Belt_Array.getExn(fs, 0), (function (a, f) {
                return function (param) {
                  return Curry._1(f, Curry._1(a, param));
                };
              }));
}

var List;

var $$Array;

export {
  List ,
  flatMapList ,
  $$Array ,
  flatMapArray ,
  foldlArray ,
  foldrArray ,
  identity ,
  eq ,
  composeU ,
  compose ,
  compose3 ,
  compose4 ,
  composeN ,
  
}
/* No side effect */
