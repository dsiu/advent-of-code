// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_SetInt from "rescript/lib/es6/belt_SetInt.js";
import * as Belt_SetString from "rescript/lib/es6/belt_SetString.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var make = Utils$AdventOfCode.splitChars;

function size(prim) {
  return prim.length;
}

function eq(a, b) {
  return Belt_SetString.eq(Belt_SetString.fromArray(a), Belt_SetString.fromArray(b));
}

function has(t, x) {
  return Belt_SetString.has(Belt_SetString.fromArray(t), x);
}

function intersect(a, b) {
  return Belt_SetString.toArray(Belt_SetString.intersect(Belt_SetString.fromArray(a), Belt_SetString.fromArray(b)));
}

function mustHave(t, must_have) {
  return eq(intersect(t, must_have), must_have);
}

function diff(a, b) {
  return Belt_SetString.toArray(Belt_SetString.diff(Belt_SetString.fromArray(a), Belt_SetString.fromArray(b)));
}

function union(a, b) {
  return Belt_SetString.toArray(Belt_SetString.union(Belt_SetString.fromArray(a), Belt_SetString.fromArray(b)));
}

function keepSize(t, size) {
  return Belt_Array.keep(t, (function (x) {
                return x.length === size;
              }));
}

var Digit = {
  make: make,
  size: size,
  map: Belt_Array.map,
  makeSet: Belt_SetString.fromArray,
  getExn: Belt_Array.getExn,
  eq: eq,
  has: has,
  intersect: intersect,
  mustHave: mustHave,
  diff: diff,
  union: union,
  keepSize: keepSize
};

var defaults = [
  [
    0,
    Utils$AdventOfCode.splitChars("abcefg")
  ],
  [
    1,
    Utils$AdventOfCode.splitChars("cf")
  ],
  [
    2,
    Utils$AdventOfCode.splitChars("acdeg")
  ],
  [
    3,
    Utils$AdventOfCode.splitChars("acdfg")
  ],
  [
    4,
    Utils$AdventOfCode.splitChars("bcdf")
  ],
  [
    5,
    Utils$AdventOfCode.splitChars("abdfg")
  ],
  [
    6,
    Utils$AdventOfCode.splitChars("abdefg")
  ],
  [
    7,
    Utils$AdventOfCode.splitChars("acf")
  ],
  [
    8,
    Utils$AdventOfCode.splitChars("abcdefg")
  ],
  [
    9,
    Utils$AdventOfCode.splitChars("abcdfg")
  ]
];

var default_0to9 = Belt_MapInt.fromArray(defaults);

function getSegments(t, n) {
  return Belt_Option.getExn(Belt_MapInt.get(t, n));
}

function getDefaultSegments(__x) {
  return Belt_Option.getExn(Belt_MapInt.get(default_0to9, __x));
}

function getSegmentSize(t, n) {
  return Belt_Option.getExn(Belt_MapInt.get(t, n)).length;
}

function getDefaultSegmentSize(__x) {
  return Belt_Option.getExn(Belt_MapInt.get(default_0to9, __x)).length;
}

function translate(t, d) {
  return Belt_Array.getExn(Belt_MapInt.keysToArray(Belt_MapInt.keep(t, (function (param, v) {
                        return eq(v, d);
                      }))), 0);
}

var Segments = {
  defaults: defaults,
  default_0to9: default_0to9,
  getSegments: getSegments,
  getDefaultSegments: getDefaultSegments,
  getSegmentSize: getSegmentSize,
  getDefaultSegmentSize: getDefaultSegmentSize,
  translate: translate
};

function find_1(xs) {
  return Belt_Array.getExn(keepSize(xs, 2), 0);
}

function find_4(xs) {
  return Belt_Array.getExn(keepSize(xs, 4), 0);
}

function find_7(xs) {
  return Belt_Array.getExn(keepSize(xs, 3), 0);
}

function find_8(xs) {
  return Belt_Array.getExn(keepSize(xs, 7), 0);
}

function find_0_6_9(xs, one, four, seven) {
  var has_6_segs = keepSize(xs, 6);
  var union_4_7 = union(four, seven);
  var nine = Belt_Array.getExn(Belt_Array.keep(has_6_segs, (function (param) {
              return mustHave(param, union_4_7);
            })), 0);
  var six_or_zero = Belt_Array.keep(has_6_segs, (function (x) {
          return !mustHave(x, union_4_7);
        }));
  var diff_4_1 = diff(four, one);
  var six = Belt_Array.getExn(Belt_Array.keep(six_or_zero, (function (param) {
              return mustHave(param, diff_4_1);
            })), 0);
  var zero = Belt_Array.getExn(Belt_Array.keep(six_or_zero, (function (x) {
              return !mustHave(x, diff_4_1);
            })), 0);
  return [
          zero,
          six,
          nine
        ];
}

function find_2_3_5(xs, eight, six, nine) {
  var has_5_segs = keepSize(xs, 5);
  var diff_8_9 = diff(eight, nine);
  var two = Belt_Array.getExn(Belt_Array.keep(has_5_segs, (function (param) {
              return mustHave(param, diff_8_9);
            })), 0);
  var three_or_five = Belt_Array.keep(has_5_segs, (function (x) {
          return !mustHave(x, diff_8_9);
        }));
  var diff_8_6 = diff(eight, six);
  var three = Belt_Array.getExn(Belt_Array.keep(three_or_five, (function (param) {
              return mustHave(param, diff_8_6);
            })), 0);
  var five = Belt_Array.getExn(Belt_Array.keep(three_or_five, (function (x) {
              return !mustHave(x, diff_8_6);
            })), 0);
  return [
          two,
          three,
          five
        ];
}

var Algo = {
  find_1: find_1,
  find_4: find_4,
  find_7: find_7,
  find_8: find_8,
  find_0_6_9: find_0_6_9,
  find_2_3_5: find_2_3_5
};

function makeSegments(xs) {
  var one = find_1(xs);
  var four = find_4(xs);
  var seven = find_7(xs);
  var eight = find_8(xs);
  var match = find_0_6_9(xs, one, four, seven);
  var nine = match[2];
  var six = match[1];
  var match$1 = find_2_3_5(xs, eight, six, nine);
  return Belt_MapInt.fromArray([
              [
                0,
                match[0]
              ],
              [
                1,
                one
              ],
              [
                2,
                match$1[0]
              ],
              [
                3,
                match$1[1]
              ],
              [
                4,
                four
              ],
              [
                5,
                match$1[2]
              ],
              [
                6,
                six
              ],
              [
                7,
                seven
              ],
              [
                8,
                eight
              ],
              [
                9,
                nine
              ]
            ]);
}

function translate$1(t, d) {
  return translate(t.segment_map, d);
}

function translateOutputs(t) {
  return Belt_Array.map(t.outputs, (function (__x) {
                return translate(t.segment_map, __x);
              }));
}

function make$1(inputStr, outputStr) {
  var inputs = Belt_Array.map(inputStr.split(" "), make);
  var outputs = Belt_Array.map(outputStr.split(" "), make);
  return {
          inputs: inputs,
          outputs: outputs,
          segment_map: makeSegments(inputs)
        };
}

function countMatchLen(digits, matches) {
  var match_len_set = Belt_SetInt.fromArray(Belt_Array.map(matches, getDefaultSegmentSize));
  return Belt_Array.reduce(digits, 0, (function (a, x) {
                if (Belt_SetInt.has(match_len_set, x.length)) {
                  return a + 1 | 0;
                } else {
                  return a;
                }
              }));
}

function countOutputMatchLen(t, matches) {
  return countMatchLen(t.outputs, matches);
}

function countInputMatchLen(t, matches) {
  return countMatchLen(t.inputs, matches);
}

var Entry = {
  Algo: Algo,
  makeSegments: makeSegments,
  translate: translate$1,
  translateOutputs: translateOutputs,
  make: make$1,
  countMatchLen: countMatchLen,
  countOutputMatchLen: countOutputMatchLen,
  countInputMatchLen: countInputMatchLen
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                var y = x.trim().split(" | ");
                return make$1(Belt_Array.get(y, 0), Belt_Array.get(y, 1));
              }));
}

function solvePart1(data) {
  var parsed = parse(data);
  return Belt_Array.reduce(parsed, 0, (function (a, x) {
                return a + countMatchLen(x.outputs, [
                            1,
                            4,
                            7,
                            8
                          ]) | 0;
              }));
}

function solvePart2(data) {
  var parsed = parse(data);
  var outputs = Belt_Array.map(parsed, (function (e) {
          return Utils$AdventOfCode.intFromStringExn(translateOutputs(e).join(""));
        }));
  return Belt_Array.reduce(outputs, 0, Utils$AdventOfCode.add);
}

export {
  log ,
  Digit ,
  Segments ,
  Entry ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* defaults Not a pure module */
