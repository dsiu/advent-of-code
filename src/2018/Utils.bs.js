// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Belt_MapString = require("bs-platform/lib/js/belt_MapString.js");
var Belt_MutableMapInt = require("bs-platform/lib/js/belt_MutableMapInt.js");

function map_string_dump(m) {
  return Belt_MapString.forEach(m, (function (k, v) {
                console.log("key:" + k + ", val:" + String(v));
                
              }));
}

function map_int_int_dump(m) {
  return Belt_MutableMapInt.forEach(m, (function (k, v) {
                console.log("key:" + String(k) + ", val:" + String(v));
                
              }));
}

function list_dump(__x) {
  return Belt_List.forEach(__x, (function (prim) {
                console.log(prim);
                
              }));
}

function flattenArray(arr) {
  return Belt_Array.reduce(arr, [], Belt_Array.concat);
}

exports.map_string_dump = map_string_dump;
exports.map_int_int_dump = map_int_int_dump;
exports.list_dump = list_dump;
exports.flattenArray = flattenArray;
/* No side effect */
