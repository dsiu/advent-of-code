// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function last(a) {
  return Belt_List.getExn(a, Belt_List.length(a) - 1 | 0);
}

function singleton(__x) {
  return Belt_List.make(1, __x);
}

function flatMap(xs, f) {
  return Belt_List.reduce(Belt_List.map(xs, f), /* [] */0, Belt_List.concat);
}

function listToOption(l) {
  if (l) {
    return Caml_option.some(l.hd);
  }
  
}

function foldLeft(xs, f) {
  var init = Belt_List.getExn(xs, 0);
  var rest = Belt_List.tailExn(xs);
  return Belt_List.reduce(rest, init, f);
}

function combinationIf2(a, b, f) {
  return Belt_List.reduceU(a, /* [] */0, (function (acc, x) {
                return Belt_List.concat(acc, Belt_List.reduceU(b, /* [] */0, (function (acc, y) {
                                  var r = f(x, y);
                                  if (r !== undefined) {
                                    return Belt_List.concat(acc, {
                                                hd: Caml_option.valFromOption(r),
                                                tl: /* [] */0
                                              });
                                  } else {
                                    return acc;
                                  }
                                })));
              }));
}

function combination2(a, b, f) {
  return combinationIf2(a, b, (function (x, y) {
                return Caml_option.some(f(x, y));
              }));
}

function unfold(p, g, b) {
  if (Curry._1(p, b)) {
    return /* [] */0;
  }
  var match = Curry._1(g, b);
  return {
          hd: match[0],
          tl: unfold(p, g, match[1])
        };
}

var length = Belt_List.length;

var size = Belt_List.size;

var head = Belt_List.head;

var headExn = Belt_List.headExn;

var tail = Belt_List.tail;

var tailExn = Belt_List.tailExn;

var add = Belt_List.add;

var get = Belt_List.get;

var getExn = Belt_List.getExn;

var make = Belt_List.make;

var makeByU = Belt_List.makeByU;

var makeBy = Belt_List.makeBy;

var shuffle = Belt_List.shuffle;

var drop = Belt_List.drop;

var take = Belt_List.take;

var splitAt = Belt_List.splitAt;

var concat = Belt_List.concat;

var concatMany = Belt_List.concatMany;

var reverseConcat = Belt_List.reverseConcat;

var flatten = Belt_List.flatten;

var mapU = Belt_List.mapU;

var map = Belt_List.map;

var zip = Belt_List.zip;

var zipByU = Belt_List.zipByU;

var zipBy = Belt_List.zipBy;

var mapWithIndexU = Belt_List.mapWithIndexU;

var mapWithIndex = Belt_List.mapWithIndex;

var fromArray = Belt_List.fromArray;

var toArray = Belt_List.toArray;

var reverse = Belt_List.reverse;

var mapReverseU = Belt_List.mapReverseU;

var mapReverse = Belt_List.mapReverse;

var forEachU = Belt_List.forEachU;

var forEach = Belt_List.forEach;

var forEachWithIndexU = Belt_List.forEachWithIndexU;

var forEachWithIndex = Belt_List.forEachWithIndex;

var reduceU = Belt_List.reduceU;

var reduce = Belt_List.reduce;

var reduceWithIndexU = Belt_List.reduceWithIndexU;

var reduceWithIndex = Belt_List.reduceWithIndex;

var reduceReverseU = Belt_List.reduceReverseU;

var reduceReverse = Belt_List.reduceReverse;

var mapReverse2U = Belt_List.mapReverse2U;

var mapReverse2 = Belt_List.mapReverse2;

var forEach2U = Belt_List.forEach2U;

var forEach2 = Belt_List.forEach2;

var reduce2U = Belt_List.reduce2U;

var reduce2 = Belt_List.reduce2;

var reduceReverse2U = Belt_List.reduceReverse2U;

var reduceReverse2 = Belt_List.reduceReverse2;

var everyU = Belt_List.everyU;

var every = Belt_List.every;

var someU = Belt_List.someU;

var some = Belt_List.some;

var every2U = Belt_List.every2U;

var every2 = Belt_List.every2;

var some2U = Belt_List.some2U;

var some2 = Belt_List.some2;

var cmpByLength = Belt_List.cmpByLength;

var cmpU = Belt_List.cmpU;

var cmp = Belt_List.cmp;

var eqU = Belt_List.eqU;

var eq = Belt_List.eq;

var hasU = Belt_List.hasU;

var has = Belt_List.has;

var getByU = Belt_List.getByU;

var getBy = Belt_List.getBy;

var keepU = Belt_List.keepU;

var keep = Belt_List.keep;

var filter = Belt_List.filter;

var keepWithIndexU = Belt_List.keepWithIndexU;

var keepWithIndex = Belt_List.keepWithIndex;

var filterWithIndex = Belt_List.filterWithIndex;

var keepMapU = Belt_List.keepMapU;

var keepMap = Belt_List.keepMap;

var partitionU = Belt_List.partitionU;

var partition = Belt_List.partition;

var unzip = Belt_List.unzip;

var getAssocU = Belt_List.getAssocU;

var getAssoc = Belt_List.getAssoc;

var hasAssocU = Belt_List.hasAssocU;

var hasAssoc = Belt_List.hasAssoc;

var removeAssocU = Belt_List.removeAssocU;

var removeAssoc = Belt_List.removeAssoc;

var setAssocU = Belt_List.setAssocU;

var setAssoc = Belt_List.setAssoc;

var sortU = Belt_List.sortU;

var sort = Belt_List.sort;

var append = Belt_List.concat;

export {
  length ,
  size ,
  head ,
  headExn ,
  tail ,
  tailExn ,
  add ,
  get ,
  getExn ,
  make ,
  makeByU ,
  makeBy ,
  shuffle ,
  drop ,
  take ,
  splitAt ,
  concat ,
  concatMany ,
  reverseConcat ,
  flatten ,
  mapU ,
  map ,
  zip ,
  zipByU ,
  zipBy ,
  mapWithIndexU ,
  mapWithIndex ,
  fromArray ,
  toArray ,
  reverse ,
  mapReverseU ,
  mapReverse ,
  forEachU ,
  forEach ,
  forEachWithIndexU ,
  forEachWithIndex ,
  reduceU ,
  reduce ,
  reduceWithIndexU ,
  reduceWithIndex ,
  reduceReverseU ,
  reduceReverse ,
  mapReverse2U ,
  mapReverse2 ,
  forEach2U ,
  forEach2 ,
  reduce2U ,
  reduce2 ,
  reduceReverse2U ,
  reduceReverse2 ,
  everyU ,
  every ,
  someU ,
  some ,
  every2U ,
  every2 ,
  some2U ,
  some2 ,
  cmpByLength ,
  cmpU ,
  cmp ,
  eqU ,
  eq ,
  hasU ,
  has ,
  getByU ,
  getBy ,
  keepU ,
  keep ,
  filter ,
  keepWithIndexU ,
  keepWithIndex ,
  filterWithIndex ,
  keepMapU ,
  keepMap ,
  partitionU ,
  partition ,
  unzip ,
  getAssocU ,
  getAssoc ,
  hasAssocU ,
  hasAssoc ,
  removeAssocU ,
  removeAssoc ,
  setAssocU ,
  setAssoc ,
  sortU ,
  sort ,
  append ,
  last ,
  singleton ,
  flatMap ,
  listToOption ,
  foldLeft ,
  combinationIf2 ,
  combination2 ,
  unfold ,
}
/* No side effect */
