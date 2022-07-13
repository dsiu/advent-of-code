// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as FP_Utils$AdventOfCode from "./FP_Utils.bs.js";

function MakeStack(Item) {
  var push = function (contents, x) {
    return /* Contents */{
            _0: {
              hd: x,
              tl: contents._0
            }
          };
  };
  var pop = function (contents) {
    var contents$1 = contents._0;
    if (contents$1) {
      var newStack = /* Contents */{
        _0: contents$1.tl
      };
      return [
              contents$1.hd,
              newStack
            ];
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
  var binary = function (mathFn, stack) {
    var match = pop(stack);
    var match$1 = pop(match[1]);
    var z = Curry._2(mathFn, match$1[0], match[0]);
    return push(match$1[1], z);
  };
  var unary = function (f, stack) {
    var match = pop(stack);
    return push(match[1], Curry._1(f, match[0]));
  };
  var show = function (stack) {
    var match = pop(stack);
    console.log(match[0]);
    return stack;
  };
  var show2 = function (stack, str) {
    var match = pop(stack);
    console.log(match[0], str);
    return stack;
  };
  var dup = function (stack) {
    var match = pop(stack);
    return push(stack, match[0]);
  };
  var swap = function (stack) {
    var match = pop(stack);
    var match$1 = pop(match[1]);
    return push(push(match$1[1], match[0]), match$1[0]);
  };
  var drop = function (stack) {
    return pop(stack)[1];
  };
  var empty = /* Contents */{
    _0: /* [] */0
  };
  var one = function (__x) {
    return push(__x, Item.one);
  };
  var two = function (__x) {
    return push(__x, Item.two);
  };
  var three = function (__x) {
    return push(__x, Item.three);
  };
  var four = function (__x) {
    return push(__x, Item.four);
  };
  var five = function (__x) {
    return push(__x, Item.five);
  };
  var mul = function (__x) {
    return binary(Item.mul, __x);
  };
  var add = function (__x) {
    return binary(Item.add, __x);
  };
  var sub = function (__x) {
    return binary(Item.sub, __x);
  };
  var div = function (__x) {
    return binary(Item.div, __x);
  };
  var neg = function (__x) {
    return unary((function (x) {
                  return Curry._1(Item.neg, x);
                }), __x);
  };
  var square = FP_Utils$AdventOfCode.composeN([
        dup,
        mul
      ]);
  var cube = FP_Utils$AdventOfCode.composeN([
        dup,
        dup,
        mul,
        mul
      ]);
  var sum_numbers_upto = FP_Utils$AdventOfCode.composeN([
        dup,
        one,
        add,
        mul,
        two,
        div
      ]);
  return {
          push: push,
          pop: pop,
          binary: binary,
          unary: unary,
          show: show,
          show2: show2,
          dup: dup,
          swap: swap,
          drop: drop,
          empty: empty,
          start: empty,
          one: one,
          two: two,
          three: three,
          four: four,
          five: five,
          mul: mul,
          add: add,
          sub: sub,
          div: div,
          neg: neg,
          square: square,
          cube: cube,
          sum_numbers_upto: sum_numbers_upto
        };
}

function add(x, y) {
  return x + y | 0;
}

function mul(x, y) {
  return Math.imul(x, y);
}

function sub(x, y) {
  return x - y | 0;
}

var div = Caml_int32.div;

function neg(x) {
  return -x | 0;
}

var IntOps = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  add: add,
  mul: mul,
  sub: sub,
  div: div,
  neg: neg
};

function push(contents, x) {
  return /* Contents */{
          _0: {
            hd: x,
            tl: contents._0
          }
        };
}

function pop(contents) {
  var contents$1 = contents._0;
  if (contents$1) {
    var newStack = /* Contents */{
      _0: contents$1.tl
    };
    return [
            contents$1.hd,
            newStack
          ];
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function binary(mathFn, stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  var z = Curry._2(mathFn, match$1[0], match[0]);
  return push(match$1[1], z);
}

function unary(f, stack) {
  var match = pop(stack);
  return push(match[1], Curry._1(f, match[0]));
}

function show(stack) {
  var match = pop(stack);
  console.log(match[0]);
  return stack;
}

function show2(stack, str) {
  var match = pop(stack);
  console.log(match[0], str);
  return stack;
}

function dup(stack) {
  var match = pop(stack);
  return push(stack, match[0]);
}

function swap(stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  return push(push(match$1[1], match[0]), match$1[0]);
}

function drop(stack) {
  return pop(stack)[1];
}

var empty = /* Contents */{
  _0: /* [] */0
};

function one(__x) {
  return push(__x, 1);
}

function two(__x) {
  return push(__x, 2);
}

function three(__x) {
  return push(__x, 3);
}

function four(__x) {
  return push(__x, 4);
}

function five(__x) {
  return push(__x, 5);
}

function mul$1(__x) {
  return binary(mul, __x);
}

function add$1(__x) {
  return binary(add, __x);
}

function sub$1(__x) {
  return binary(sub, __x);
}

function div$1(__x) {
  return binary(div, __x);
}

function neg$1(__x) {
  return unary((function (x) {
                return -x | 0;
              }), __x);
}

var square = FP_Utils$AdventOfCode.composeN([
      dup,
      mul$1
    ]);

var cube = FP_Utils$AdventOfCode.composeN([
      dup,
      dup,
      mul$1,
      mul$1
    ]);

var sum_numbers_upto = FP_Utils$AdventOfCode.composeN([
      dup,
      one,
      add$1,
      mul$1,
      two,
      div$1
    ]);

var StackInt = {
  push: push,
  pop: pop,
  binary: binary,
  unary: unary,
  show: show,
  show2: show2,
  dup: dup,
  swap: swap,
  drop: drop,
  empty: empty,
  start: empty,
  one: one,
  two: two,
  three: three,
  four: four,
  five: five,
  mul: mul$1,
  add: add$1,
  sub: sub$1,
  div: div$1,
  neg: neg$1,
  square: square,
  cube: cube,
  sum_numbers_upto: sum_numbers_upto
};

function add$2(x, y) {
  return x + y;
}

function mul$2(x, y) {
  return x * y;
}

function sub$2(x, y) {
  return x - y;
}

function div$2(x, y) {
  return x / y;
}

function neg$2(x) {
  return 0.0 - x;
}

var FloatOps = {
  one: 1.0,
  two: 2.0,
  three: 3.0,
  four: 4.0,
  five: 5.0,
  add: add$2,
  mul: mul$2,
  sub: sub$2,
  div: div$2,
  neg: neg$2
};

function push$1(contents, x) {
  return /* Contents */{
          _0: {
            hd: x,
            tl: contents._0
          }
        };
}

function pop$1(contents) {
  var contents$1 = contents._0;
  if (contents$1) {
    var newStack = /* Contents */{
      _0: contents$1.tl
    };
    return [
            contents$1.hd,
            newStack
          ];
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function binary$1(mathFn, stack) {
  var match = pop$1(stack);
  var match$1 = pop$1(match[1]);
  var z = Curry._2(mathFn, match$1[0], match[0]);
  return push$1(match$1[1], z);
}

function unary$1(f, stack) {
  var match = pop$1(stack);
  return push$1(match[1], Curry._1(f, match[0]));
}

function show$1(stack) {
  var match = pop$1(stack);
  console.log(match[0]);
  return stack;
}

function show2$1(stack, str) {
  var match = pop$1(stack);
  console.log(match[0], str);
  return stack;
}

function dup$1(stack) {
  var match = pop$1(stack);
  return push$1(stack, match[0]);
}

function swap$1(stack) {
  var match = pop$1(stack);
  var match$1 = pop$1(match[1]);
  return push$1(push$1(match$1[1], match[0]), match$1[0]);
}

function drop$1(stack) {
  return pop$1(stack)[1];
}

var empty$1 = /* Contents */{
  _0: /* [] */0
};

function one$1(__x) {
  return push$1(__x, 1.0);
}

function two$1(__x) {
  return push$1(__x, 2.0);
}

function three$1(__x) {
  return push$1(__x, 3.0);
}

function four$1(__x) {
  return push$1(__x, 4.0);
}

function five$1(__x) {
  return push$1(__x, 5.0);
}

function mul$3(__x) {
  return binary$1(mul$2, __x);
}

function add$3(__x) {
  return binary$1(add$2, __x);
}

function sub$3(__x) {
  return binary$1(sub$2, __x);
}

function div$3(__x) {
  return binary$1(div$2, __x);
}

function neg$3(__x) {
  return unary$1((function (x) {
                return 0.0 - x;
              }), __x);
}

var square$1 = FP_Utils$AdventOfCode.composeN([
      dup$1,
      mul$3
    ]);

var cube$1 = FP_Utils$AdventOfCode.composeN([
      dup$1,
      dup$1,
      mul$3,
      mul$3
    ]);

var sum_numbers_upto$1 = FP_Utils$AdventOfCode.composeN([
      dup$1,
      one$1,
      add$3,
      mul$3,
      two$1,
      div$3
    ]);

var StackFloat = {
  push: push$1,
  pop: pop$1,
  binary: binary$1,
  unary: unary$1,
  show: show$1,
  show2: show2$1,
  dup: dup$1,
  swap: swap$1,
  drop: drop$1,
  empty: empty$1,
  start: empty$1,
  one: one$1,
  two: two$1,
  three: three$1,
  four: four$1,
  five: five$1,
  mul: mul$3,
  add: add$3,
  sub: sub$3,
  div: div$3,
  neg: neg$3,
  square: square$1,
  cube: cube$1,
  sum_numbers_upto: sum_numbers_upto$1
};

export {
  MakeStack ,
  IntOps ,
  StackInt ,
  FloatOps ,
  StackFloat ,
  
}
/* square Not a pure module */
