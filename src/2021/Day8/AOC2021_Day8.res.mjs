// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Utils from "../../Utils.res.mjs";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_MapInt from "rescript/lib/es6/Belt_MapInt.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Belt_SetInt from "rescript/lib/es6/Belt_SetInt.js";
import * as Belt_SetString from "rescript/lib/es6/Belt_SetString.js";

let make = Utils.splitChars;

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
  return Belt_Array.keep(t, x => x.length === size);
}

let Digit = {
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

let defaults = [
  [
    0,
    Utils.splitChars("abcefg")
  ],
  [
    1,
    Utils.splitChars("cf")
  ],
  [
    2,
    Utils.splitChars("acdeg")
  ],
  [
    3,
    Utils.splitChars("acdfg")
  ],
  [
    4,
    Utils.splitChars("bcdf")
  ],
  [
    5,
    Utils.splitChars("abdfg")
  ],
  [
    6,
    Utils.splitChars("abdefg")
  ],
  [
    7,
    Utils.splitChars("acf")
  ],
  [
    8,
    Utils.splitChars("abcdefg")
  ],
  [
    9,
    Utils.splitChars("abcdfg")
  ]
];

let default_0to9 = Belt_MapInt.fromArray(defaults);

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
  return Belt_Array.getExn(Belt_MapInt.keysToArray(Belt_MapInt.keep(t, (param, v) => eq(v, d))), 0);
}

let Segments = {
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
  let has_6_segs = keepSize(xs, 6);
  let union_4_7 = union(four, seven);
  let nine = Belt_Array.getExn(Belt_Array.keep(has_6_segs, __x => mustHave(__x, union_4_7)), 0);
  let six_or_zero = Belt_Array.keep(has_6_segs, x => !mustHave(x, union_4_7));
  let diff_4_1 = diff(four, one);
  let six = Belt_Array.getExn(Belt_Array.keep(six_or_zero, __x => mustHave(__x, diff_4_1)), 0);
  let zero = Belt_Array.getExn(Belt_Array.keep(six_or_zero, x => !mustHave(x, diff_4_1)), 0);
  return [
    zero,
    six,
    nine
  ];
}

function find_2_3_5(xs, eight, six, nine) {
  let has_5_segs = keepSize(xs, 5);
  let diff_8_9 = diff(eight, nine);
  let two = Belt_Array.getExn(Belt_Array.keep(has_5_segs, __x => mustHave(__x, diff_8_9)), 0);
  let three_or_five = Belt_Array.keep(has_5_segs, x => !mustHave(x, diff_8_9));
  let diff_8_6 = diff(eight, six);
  let three = Belt_Array.getExn(Belt_Array.keep(three_or_five, __x => mustHave(__x, diff_8_6)), 0);
  let five = Belt_Array.getExn(Belt_Array.keep(three_or_five, x => !mustHave(x, diff_8_6)), 0);
  return [
    two,
    three,
    five
  ];
}

let Algo = {
  find_1: find_1,
  find_4: find_4,
  find_7: find_7,
  find_8: find_8,
  find_0_6_9: find_0_6_9,
  find_2_3_5: find_2_3_5
};

function makeSegments(xs) {
  let one = find_1(xs);
  let four = find_4(xs);
  let seven = find_7(xs);
  let eight = find_8(xs);
  let match = find_0_6_9(xs, one, four, seven);
  let nine = match[2];
  let six = match[1];
  let match$1 = find_2_3_5(xs, eight, six, nine);
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
  return Belt_Array.map(t.outputs, __x => translate(t.segment_map, __x));
}

function make$1(inputStr, outputStr) {
  let inputs = Belt_Array.map(inputStr.split(" "), make);
  let outputs = Belt_Array.map(outputStr.split(" "), make);
  return {
    inputs: inputs,
    outputs: outputs,
    segment_map: makeSegments(inputs)
  };
}

function countMatchLen(digits, matches) {
  let match_len_set = Belt_SetInt.fromArray(Belt_Array.map(matches, x => Belt_Option.getExn(Belt_MapInt.get(default_0to9, x)).length));
  return Belt_Array.reduce(digits, 0, (a, x) => {
    if (Belt_SetInt.has(match_len_set, x.length)) {
      return a + 1 | 0;
    } else {
      return a;
    }
  });
}

function countOutputMatchLen(t, matches) {
  return countMatchLen(t.outputs, matches);
}

function countInputMatchLen(t, matches) {
  return countMatchLen(t.inputs, matches);
}

let Entry = {
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
  return Belt_Array.map(Utils.splitNewline(data), x => {
    let y = x.trim().split(" | ");
    return make$1(Belt_Array.get(y, 0), Belt_Array.get(y, 1));
  });
}

function solvePart1(data) {
  let parsed = parse(data);
  return Belt_Array.reduce(parsed, 0, (a, x) => a + countMatchLen(x.outputs, [
    1,
    4,
    7,
    8
  ]) | 0);
}

function solvePart2(data) {
  let parsed = parse(data);
  let outputs = Belt_Array.map(parsed, e => Utils.intFromStringExn(translateOutputs(e).join("")));
  return Belt_Array.reduce(outputs, 0, Utils.add);
}

export {
  Digit,
  Segments,
  Entry,
  parse,
  solvePart1,
  solvePart2,
}
/* defaults Not a pure module */