// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Map from "rescript/lib/es6/Belt_Map.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Result from "@dsiu/rescript-stdlib-fp/src/Stdlib__Result.mjs";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.mjs";
import * as ReludeParse_Parser from "relude-parse/src/ReludeParse_Parser.mjs";
import * as TableclothComparator from "@dsiu/rescript-stdlib-fp/src/Tablecloth/TableclothComparator.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

let compare = Primitive_object.compare;

let include = TableclothComparator.Make({
  compare: compare
});

let names = Stdlib__List.fromArray("23456789TJQKA".split(""));

function make(str) {
  switch (str) {
    case "2" :
      return "Two";
    case "3" :
      return "Three";
    case "4" :
      return "Four";
    case "5" :
      return "Five";
    case "6" :
      return "Six";
    case "7" :
      return "Seven";
    case "8" :
      return "Eight";
    case "9" :
      return "Nine";
    case "A" :
      return "Ace";
    case "J" :
      return "Jack";
    case "K" :
      return "King";
    case "Q" :
      return "Queen";
    case "T" :
      return "Ten";
    default:
      return Pervasives.failwith("invalid card");
  }
}

function value(t) {
  switch (t) {
    case "Joker" :
      return 1;
    case "Two" :
      return 2;
    case "Three" :
      return 3;
    case "Four" :
      return 4;
    case "Five" :
      return 5;
    case "Six" :
      return 6;
    case "Seven" :
      return 7;
    case "Eight" :
      return 8;
    case "Nine" :
      return 9;
    case "Ten" :
      return 10;
    case "Jack" :
      return 11;
    case "Queen" :
      return 12;
    case "King" :
      return 13;
    case "Ace" :
      return 14;
  }
}

function compare_Ord(a, b) {
  return Primitive_int.compare(value(a), value(b));
}

function compare$1(a, b) {
  return Stdlib__Ordering.toInt(compare_Ord(a, b));
}

let Card_comparator = include.comparator;

let Card = {
  comparator: Card_comparator,
  names: names,
  make: make,
  value: value,
  compare_Ord: compare_Ord,
  compare: compare$1
};

function value$1(t) {
  switch (t) {
    case "HighCard" :
      return 1;
    case "OnePair" :
      return 2;
    case "TwoPair" :
      return 3;
    case "ThreeOfAKind" :
      return 4;
    case "FullHouse" :
      return 5;
    case "FourOfAKind" :
      return 6;
    case "FiveOfAKind" :
      return 7;
  }
}

function compare_Ord$1(a, b) {
  return Primitive_int.compare(value$1(a), value$1(b));
}

function compare$2(a, b) {
  return Stdlib__Ordering.toInt(compare_Ord$1(a, b));
}

let HandClass = {
  value: value$1,
  compare_Ord: compare_Ord$1,
  compare: compare$2
};

function signatureElementCompare(param, param$1) {
  let n = Primitive_int.compare(param[0], param$1[0]);
  if (Stdlib__Ordering.isEqual(n)) {
    return compare_Ord(Stdlib__Array.getUnsafe(param[1], 0), Stdlib__Array.getUnsafe(param$1[1], 0));
  } else {
    return n;
  }
}

function sign(cards) {
  let innerSign = cards => Belt_Map.valuesToArray(Stdlib__Array.groupBy(cards.toSorted(compare_Ord), Card, a => a)).map(group => [
    Stdlib__List.length(group),
    Stdlib__List.toArray(group)
  ]).toSorted(signatureElementCompare).toReversed();
  let match = Stdlib__Array.partition(cards, c => c === "Joker");
  let jokers = match[0];
  let nonJokerSigned = innerSign(match[1]);
  let js = [
    jokers.length,
    jokers
  ];
  if (nonJokerSigned.length === 0) {
    return [js];
  }
  let match$1 = Stdlib__Array.getUnsafe(nonJokerSigned, 0);
  return [[
      match$1[0] + js[0] | 0,
      match$1[1].concat(js[1])
    ]].concat(Stdlib__Array.tail(nonJokerSigned));
}

function classifySignature(signature) {
  let len = signature.length;
  if (len >= 5) {
    return "HighCard";
  }
  switch (len) {
    case 0 :
      return "HighCard";
    case 1 :
      let match = signature[0];
      if (match[0] !== 5) {
        return "HighCard";
      } else {
        return "FiveOfAKind";
      }
    case 2 :
      let match$1 = signature[0];
      let match$2 = match$1[0];
      if (match$2 !== 3) {
        if (match$2 !== 4) {
          return "HighCard";
        } else {
          return "FourOfAKind";
        }
      }
      let match$3 = signature[1];
      if (match$3[0] !== 2) {
        return "ThreeOfAKind";
      } else {
        return "FullHouse";
      }
    case 3 :
      let match$4 = signature[0];
      let match$5 = match$4[0];
      if (match$5 !== 2) {
        if (match$5 !== 3) {
          return "HighCard";
        } else {
          return "ThreeOfAKind";
        }
      }
      let match$6 = signature[1];
      if (match$6[0] !== 2) {
        return "HighCard";
      } else {
        return "TwoPair";
      }
    case 4 :
      let match$7 = signature[0];
      if (match$7[0] !== 2) {
        return "HighCard";
      } else {
        return "OnePair";
      }
  }
}

function classify(param) {
  let cards = param._0;
  return {
    TAG: "CHand",
    _0: classifySignature(sign(cards)),
    _1: cards,
    _2: param._1
  };
}

function part1(hands) {
  let sortedHands = hands.map(classify).toSorted((param, param$1) => {
    let h = compare_Ord$1(param._0, param$1._0);
    if (Stdlib__Ordering.isEqual(h)) {
      return Stdlib__Ordering.invert(Stdlib__Array.compare(param._1, param$1._1, (d, e) => Stdlib__Ordering.invert(compare_Ord(d, e))));
    } else {
      return h;
    }
  });
  let rankedHands = Stdlib__Array.zip(Stdlib__Array.range(1, sortedHands.length + 1 | 0), sortedHands);
  let score = param => Math.imul(param[0], param[1]._2);
  return Stdlib__Array.sum(rankedHands.map(score), {
    zero: Stdlib__Int.zero,
    add: Stdlib__Int.add
  });
}

function enJoker(param) {
  let replaceJackWithJoker = card => {
    if (card === "Jack") {
      return "Joker";
    } else {
      return card;
    }
  };
  return {
    TAG: "Hand",
    _0: param._0.map(replaceJackWithJoker),
    _1: param._1
  };
}

function part2(hands) {
  return part1(hands.map(enJoker));
}

let justSpace = ReludeParse_Parser.$$void(ReludeParse_Parser.many(ReludeParse_Parser.str(" ")));

function handC(a) {
  return b => {
    let cards = Stdlib__List.toArray(a);
    return {
      TAG: "Hand",
      _0: cards,
      _1: b
    };
  };
}

let cardP = ReludeParse_Parser.Infix.$less$$great(make, ReludeParse_Parser.anyOfStr(names));

let handP = ReludeParse_Parser.Infix.$less$star$great(ReludeParse_Parser.Infix.$less$$great(handC, ReludeParse_Parser.Infix.$star$great(justSpace, ReludeParse_Parser.Infix.$less$star(ReludeParse_Parser.many(cardP), justSpace))), ReludeParse_Parser.anyInt);

let handsP = ReludeParse_Parser.sepBy(ReludeParse_Parser.eol, handP);

function run(str) {
  return Stdlib__List.toArray(Stdlib__Result.getExn(ReludeParse_Parser.runParser(str, handsP)));
}

let HandsParser = {
  P: undefined,
  justSpace: justSpace,
  handC: handC,
  cardP: cardP,
  handP: handP,
  handsP: handsP,
  run: run
};

function solvePart1(data) {
  return part1(run(data));
}

function solvePart2(data) {
  let hands = run(data);
  return part1(hands.map(enJoker));
}

export {
  log,
  log2,
  Card,
  HandClass,
  signatureElementCompare,
  sign,
  classifySignature,
  classify,
  part1,
  enJoker,
  part2,
  HandsParser,
  solvePart1,
  solvePart2,
}
/* include Not a pure module */
