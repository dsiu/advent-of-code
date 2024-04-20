// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int64 from "rescript/lib/es6/int64.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as JsArray2Ex from "js-array2-ex/src/JsArray2Ex.mjs";
import * as Belt_MapInt from "rescript/lib/es6/belt_MapInt.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Belt_HashMapString from "rescript/lib/es6/belt_HashMapString.js";
import * as Belt_MutableMapInt from "rescript/lib/es6/belt_MutableMapInt.js";
import * as Belt_MutableMapString from "rescript/lib/es6/belt_MutableMapString.js";

function identity(prim) {
  return prim;
}

function log(prim) {
  console.log(prim);
}

function toString(m, f) {
  return Belt_MapString.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k + ", val:" + f(v) + "\n");
              }));
}

function toString$1(m) {
  return toString(m, identity);
}

var $$String = {
  toString: toString$1
};

(1).toString();

function toString$2(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var Int = {
  toString: toString$2
};

var MapString = {
  toString: toString,
  $$String: $$String,
  Int: Int
};

function toString$3(m, f) {
  return Belt_MapInt.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k.toString() + ", val:" + f(v) + "\n");
              }));
}

function toString$4(m) {
  return toString$3(m, identity);
}

var $$String$1 = {
  toString: toString$4
};

function toString$5(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$3(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var Int$1 = {
  toString: toString$5
};

function toString$6(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$3(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var $$BigInt = {
  toString: toString$6
};

var MapInt = {
  toString: toString$3,
  $$String: $$String$1,
  Int: Int$1,
  $$BigInt: $$BigInt
};

function toString$7(m, f) {
  return Belt_MutableMapInt.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k.toString() + ", val:" + f(v) + "\n");
              }));
}

function toString$8(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$7(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var Int$2 = {
  toString: toString$8
};

function toString$9(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$7(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var $$BigInt$1 = {
  toString: toString$9
};

var MutableMapInt = {
  toString: toString$7,
  Int: Int$2,
  $$BigInt: $$BigInt$1
};

function toString$10(m, f) {
  return Belt_MutableMapString.reduce(m, "", (function (a, k, v) {
                return a + ("key:" + k + ", val:" + f(v) + "\n");
              }));
}

function toString$11(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$10(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var Int$3 = {
  toString: toString$11
};

function toString$12(m, radixOpt) {
  var radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$10(m, (function (__x) {
                return __x.toString(radix);
              }));
}

var $$BigInt$2 = {
  toString: toString$12
};

function toString$13(m) {
  return toString$10(m, Int64.to_string);
}

var Int64$1 = {
  toString: toString$13
};

var MutableMapString = {
  toString: toString$10,
  Int: Int$3,
  $$BigInt: $$BigInt$2,
  Int64: Int64$1
};

function toString$14(a, f) {
  return "[" + a.map(f).join(",") + "]";
}

var $$Array = {
  toString: toString$14
};

function toString$15(a, f) {
  return Stdlib__List.reduce(a, "{", (function (a, v) {
                return a + f(v) + ",";
              })) + "}";
}

var List = {
  toString: toString$15
};

var Printable = {
  MapString: MapString,
  MapInt: MapInt,
  MutableMapInt: MutableMapInt,
  MutableMapString: MutableMapString,
  $$Array: $$Array,
  List: List
};

function base2(__x) {
  return __x.toString(2);
}

function compose(f, g) {
  return function (extra) {
    return Stdlib__Function.compose(f, g, extra);
  };
}

function g(prim) {
  return prim;
}

function f(none) {
  return Stdlib__Int.fromString(none, 10);
}

function g$1(extra) {
  return Stdlib__Function.compose(f, g, extra);
}

function f$1(prim) {
  return prim.trim();
}

function intFromStringExn(extra) {
  return Stdlib__Function.compose(f$1, g$1, extra);
}

function add(x, y) {
  return x + y | 0;
}

function sub(x, y) {
  return x - y | 0;
}

function mul(x, y) {
  return Math.imul(x, y);
}

var div = Caml_int32.div;

function int32ToUint32(x) {
  return new Uint32Array([x])[0];
}

function increaseByInt64(v, n) {
  return Stdlib__Option.mapOr(v, n, (function (x) {
                return Caml_int64.add(x, n);
              }));
}

function increaseBy1L(__x) {
  return increaseByInt64(__x, Caml_int64.one);
}

function increaseBy(v, n) {
  return Stdlib__Option.mapOr(v, n, (function (x) {
                return x + n | 0;
              }));
}

function increaseBy1(__x) {
  return increaseBy(__x, 1);
}

function int64FromBitString(str) {
  return Caml_format.int64_of_string("0b" + str);
}

function splitChars(__x) {
  return __x.split("");
}

function splitSpace(__x) {
  return __x.split(" ");
}

function splitNewline(__x) {
  return __x.split("\n");
}

function splitDoubleNewline(__x) {
  return __x.split("\n\n");
}

function sumIntArray(__x) {
  return Stdlib__Array.reduce(__x, 0, add);
}

function mulIntArray(__x) {
  return Stdlib__Array.reduce(__x, 1, mul);
}

function join(__x) {
  return __x.join("");
}

function sumRange(xs, offset, len) {
  var elems = Belt_Array.slice(xs, offset, len);
  var total = {
    contents: 0
  };
  elems.forEach(function (x) {
        total.contents = total.contents + x | 0;
      });
  return total.contents;
}

function maxIntInArray(xs) {
  var sorted = xs.toSorted(Stdlib__Int.compare);
  return Stdlib__Array.getUnsafe(sorted, sorted.length - 1 | 0);
}

function minIntInArray(xs) {
  var sorted = xs.toSorted(Stdlib__Int.compare);
  return Stdlib__Array.getUnsafe(sorted, 0);
}

function compare(a, b) {
  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
}

var BigIntExt = {
  compare: compare
};

function maxBigIntInArray(xs) {
  var sorted = xs.toSorted(compare);
  return Stdlib__Array.getUnsafe(sorted, sorted.length - 1 | 0);
}

function minBigIntInArray(xs) {
  var sorted = xs.toSorted(compare);
  return Stdlib__Array.getUnsafe(sorted, 0);
}

function flatten(xs) {
  return Stdlib__Array.reduce(xs, [], (function (a, x) {
                return a.concat(x);
              }));
}

function maxKeyIntValuePair(__x) {
  return Stdlib__Array.reduce(__x, [
              "",
              PervasivesU.min_int
            ], (function (acc, param) {
                var v = param[1];
                if (v > acc[1]) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function minKeyIntValuePair(__x) {
  return Stdlib__Array.reduce(__x, [
              "",
              PervasivesU.max_int
            ], (function (acc, param) {
                var v = param[1];
                if (v < acc[1]) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function maxKeyInt64ValuePair(__x) {
  return Stdlib__Array.reduce(__x, [
              "",
              Int64.min_int
            ], (function (acc, param) {
                var v = param[1];
                if (Int64.compare(v, acc[1]) > 0) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function minKeyInt64ValuePair(__x) {
  return Stdlib__Array.reduce(__x, [
              "",
              Int64.max_int
            ], (function (acc, param) {
                var v = param[1];
                if (Int64.compare(v, acc[1]) < 0) {
                  return [
                          param[0],
                          v
                        ];
                } else {
                  return acc;
                }
              }));
}

function hashMapStringUpdate(h, k, f) {
  Belt_HashMapString.set(h, k, Stdlib__Option.mapOr(Belt_HashMapString.get(h, k), f(undefined), (function (x) {
              return f(Caml_option.some(x));
            })));
  return h;
}

function mutableMapStringUpdate(h, k, f) {
  Belt_MutableMapString.set(h, k, Stdlib__Option.mapOr(Belt_MutableMapString.get(h, k), f(undefined), (function (x) {
              return f(Caml_option.some(x));
            })));
  return h;
}

var transpose = JsArray2Ex.transpose;

export {
  identity ,
  log ,
  Printable ,
  base2 ,
  compose ,
  intFromStringExn ,
  add ,
  sub ,
  mul ,
  div ,
  int32ToUint32 ,
  increaseByInt64 ,
  increaseBy1L ,
  increaseBy ,
  increaseBy1 ,
  int64FromBitString ,
  splitChars ,
  splitSpace ,
  splitNewline ,
  splitDoubleNewline ,
  sumIntArray ,
  mulIntArray ,
  join ,
  sumRange ,
  maxIntInArray ,
  minIntInArray ,
  BigIntExt ,
  maxBigIntInArray ,
  minBigIntInArray ,
  flatten ,
  transpose ,
  maxKeyIntValuePair ,
  minKeyIntValuePair ,
  maxKeyInt64ValuePair ,
  minKeyInt64ValuePair ,
  hashMapStringUpdate ,
  mutableMapStringUpdate ,
}
/*  Not a pure module */
