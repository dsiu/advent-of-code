// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var List = require("bs-platform/lib/js/list.js");
var Lens$Rationale = require("rationale/src/Lens.bs.js");

var record = {
  basic: 1,
  nest: {
    innerBasic: 2,
    innerSome: 3,
    innerNone: undefined
  },
  some: 4,
  none: undefined,
  someNest: {
    innerBasic: 5,
    innerSome: 6,
    innerNone: undefined
  },
  noneNest: undefined,
  list: {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: /* [] */0
      }
    }
  },
  emptyList: /* [] */0,
  dict: {
    hd: [
      "a",
      7
    ],
    tl: /* [] */0
  },
  tuple: [
    8,
    9
  ]
};

var basicLens = Lens$Rationale.make((function (a) {
        return a.basic;
      }), (function (v, a) {
        return {
                basic: v,
                nest: a.nest,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: a.list,
                emptyList: a.emptyList,
                dict: a.dict,
                tuple: a.tuple
              };
      }));

var innerBasicLens = Lens$Rationale.make((function (a) {
        return a.innerBasic;
      }), (function (v, a) {
        return {
                innerBasic: v,
                innerSome: a.innerSome,
                innerNone: a.innerNone
              };
      }));

var nestLens = Lens$Rationale.make((function (a) {
        return a.nest;
      }), (function (v, a) {
        return {
                basic: a.basic,
                nest: v,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: a.list,
                emptyList: a.emptyList,
                dict: a.dict,
                tuple: a.tuple
              };
      }));

var someLens = Lens$Rationale.$great$great$neg(Lens$Rationale.make((function (a) {
            return a.some;
          }), (function (v, a) {
            return {
                    basic: a.basic,
                    nest: a.nest,
                    some: v,
                    none: a.none,
                    someNest: a.someNest,
                    noneNest: a.noneNest,
                    list: a.list,
                    emptyList: a.emptyList,
                    dict: a.dict,
                    tuple: a.tuple
                  };
          })), Lens$Rationale.optional(0));

var noneLens = Lens$Rationale.$great$great$neg(Lens$Rationale.make((function (a) {
            return a.none;
          }), (function (v, a) {
            return {
                    basic: a.basic,
                    nest: a.nest,
                    some: a.some,
                    none: v,
                    someNest: a.someNest,
                    noneNest: a.noneNest,
                    list: a.list,
                    emptyList: a.emptyList,
                    dict: a.dict,
                    tuple: a.tuple
                  };
          })), Lens$Rationale.optional(0));

var someNestLens = Lens$Rationale.$great$great$neg(Lens$Rationale.make((function (a) {
            return a.someNest;
          }), (function (v, a) {
            return {
                    basic: a.basic,
                    nest: a.nest,
                    some: a.some,
                    none: a.none,
                    someNest: v,
                    noneNest: a.noneNest,
                    list: a.list,
                    emptyList: a.emptyList,
                    dict: a.dict,
                    tuple: a.tuple
                  };
          })), Lens$Rationale.optional({
          innerBasic: 0,
          innerSome: 0,
          innerNone: undefined
        }));

var noneNestLens = Lens$Rationale.$great$great$neg(Lens$Rationale.make((function (a) {
            return a.noneNest;
          }), (function (v, a) {
            return {
                    basic: a.basic,
                    nest: a.nest,
                    some: a.some,
                    none: a.none,
                    someNest: v,
                    noneNest: a.noneNest,
                    list: a.list,
                    emptyList: a.emptyList,
                    dict: a.dict,
                    tuple: a.tuple
                  };
          })), Lens$Rationale.optional({
          innerBasic: 0,
          innerSome: 0,
          innerNone: undefined
        }));

var listLens = Lens$Rationale.make((function (a) {
        return a.list;
      }), (function (v, a) {
        return {
                basic: a.basic,
                nest: a.nest,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: v,
                emptyList: a.emptyList,
                dict: a.dict,
                tuple: a.tuple
              };
      }));

var emptyListLens = Lens$Rationale.make((function (a) {
        return a.emptyList;
      }), (function (v, a) {
        return {
                basic: a.basic,
                nest: a.nest,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: a.list,
                emptyList: v,
                dict: a.dict,
                tuple: a.tuple
              };
      }));

var dictLens = Lens$Rationale.make((function (a) {
        return a.dict;
      }), (function (v, a) {
        return {
                basic: a.basic,
                nest: a.nest,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: a.list,
                emptyList: a.emptyList,
                dict: v,
                tuple: a.tuple
              };
      }));

var aLens = Lens$Rationale.$great$great$neg(Lens$Rationale.prop("a"), Lens$Rationale.optional(0));

var tupleLens = Lens$Rationale.make((function (a) {
        return a.tuple;
      }), (function (v, a) {
        return {
                basic: a.basic,
                nest: a.nest,
                some: a.some,
                none: a.none,
                someNest: a.someNest,
                noneNest: a.noneNest,
                list: a.list,
                emptyList: a.emptyList,
                dict: a.dict,
                tuple: v
              };
      }));

Jest.describe("view", (function (param) {
        Jest.test("basic", (function (param) {
                return Jest.Expect.toEqual(1, Jest.Expect.expect(Lens$Rationale.view(basicLens, record)));
              }));
        Jest.test("nest", (function (param) {
                return Jest.Expect.toEqual(2, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(nestLens, innerBasicLens), record)));
              }));
        Jest.test("some", (function (param) {
                return Jest.Expect.toEqual(4, Jest.Expect.expect(Lens$Rationale.view(someLens, record)));
              }));
        Jest.test("none", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Lens$Rationale.view(noneLens, record)));
              }));
        Jest.test("someNest", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(someNestLens, innerBasicLens), record)));
              }));
        Jest.test("noneNest", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(noneNestLens, innerBasicLens), record)));
              }));
        Jest.describe("list", (function (param) {
                Jest.test("head", (function (param) {
                        return Jest.Expect.toEqual(1, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.head), Lens$Rationale.optional(0)), record)));
                      }));
                Jest.test("tail", (function (param) {
                        return Jest.Expect.toEqual({
                                    hd: 2,
                                    tl: {
                                      hd: 3,
                                      tl: /* [] */0
                                    }
                                  }, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.tail), Lens$Rationale.optional({
                                                    hd: 0,
                                                    tl: /* [] */0
                                                  })), record)));
                      }));
                return Jest.test("index", (function (param) {
                              return Jest.Expect.toEqual(2, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.index(1)), Lens$Rationale.optional(0)), record)));
                            }));
              }));
        Jest.test("dict", (function (param) {
                return Jest.Expect.toEqual(7, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(dictLens, aLens), record)));
              }));
        return Jest.describe("tuple", (function (param) {
                      Jest.test("first", (function (param) {
                              return Jest.Expect.toEqual(8, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.first), record)));
                            }));
                      return Jest.test("second", (function (param) {
                                    return Jest.Expect.toEqual(9, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.second), record)));
                                  }));
                    }));
      }));

Jest.describe("set", (function (param) {
        Jest.test("basic", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(basicLens, Lens$Rationale.set(basicLens, 5, record))));
              }));
        Jest.test("nest", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(nestLens, innerBasicLens), Lens$Rationale.set(Lens$Rationale.$great$great$neg(nestLens, innerBasicLens), 5, record))));
              }));
        Jest.test("some", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(someLens, Lens$Rationale.set(someLens, 5, record))));
              }));
        Jest.test("none", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(noneLens, Lens$Rationale.set(noneLens, 5, record))));
              }));
        Jest.test("someNest", (function (param) {
                return Jest.Expect.toEqual(7, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(someNestLens, innerBasicLens), Lens$Rationale.set(Lens$Rationale.$great$great$neg(someNestLens, innerBasicLens), 7, record))));
              }));
        Jest.test("noneNest", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(noneNestLens, innerBasicLens), Lens$Rationale.set(Lens$Rationale.$great$great$neg(noneNestLens, innerBasicLens), 5, record))));
              }));
        Jest.describe("list", (function (param) {
                Jest.test("head", (function (param) {
                        return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.head), Lens$Rationale.optional(0)), Lens$Rationale.set(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.head), Lens$Rationale.optional(0)), 5, record))));
                      }));
                Jest.test("tail", (function (param) {
                        return Jest.Expect.toEqual({
                                    hd: 5,
                                    tl: {
                                      hd: 5,
                                      tl: /* [] */0
                                    }
                                  }, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.tail), Lens$Rationale.optional({
                                                    hd: 0,
                                                    tl: /* [] */0
                                                  })), Lens$Rationale.set(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.tail), Lens$Rationale.optional({
                                                        hd: 0,
                                                        tl: /* [] */0
                                                      })), {
                                                hd: 5,
                                                tl: {
                                                  hd: 5,
                                                  tl: /* [] */0
                                                }
                                              }, record))));
                      }));
                return Jest.test("index", (function (param) {
                              return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.index(1)), Lens$Rationale.optional(0)), Lens$Rationale.set(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.index(1)), Lens$Rationale.optional(0)), 5, record))));
                            }));
              }));
        Jest.test("dict", (function (param) {
                return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(dictLens, aLens), Lens$Rationale.set(Lens$Rationale.$great$great$neg(dictLens, aLens), 5, record))));
              }));
        return Jest.describe("tuple", (function (param) {
                      Jest.test("first", (function (param) {
                              return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.first), Lens$Rationale.set(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.first), 5, record))));
                            }));
                      return Jest.test("second", (function (param) {
                                    return Jest.Expect.toEqual(5, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.second), Lens$Rationale.set(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.second), 5, record))));
                                  }));
                    }));
      }));

Jest.describe("over", (function (param) {
        var $$double = function (x) {
          return (x << 1);
        };
        Jest.test("basic", (function (param) {
                return Jest.Expect.toEqual(2, Jest.Expect.expect(Lens$Rationale.view(basicLens, Lens$Rationale.over(basicLens, $$double, record))));
              }));
        Jest.test("nest", (function (param) {
                return Jest.Expect.toEqual(4, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(nestLens, innerBasicLens), Lens$Rationale.over(Lens$Rationale.$great$great$neg(nestLens, innerBasicLens), $$double, record))));
              }));
        Jest.test("some", (function (param) {
                return Jest.Expect.toEqual(8, Jest.Expect.expect(Lens$Rationale.view(someLens, Lens$Rationale.over(someLens, $$double, record))));
              }));
        Jest.test("none", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Lens$Rationale.view(noneLens, Lens$Rationale.over(noneLens, $$double, record))));
              }));
        Jest.test("someNest", (function (param) {
                return Jest.Expect.toEqual(10, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(someNestLens, innerBasicLens), Lens$Rationale.over(Lens$Rationale.$great$great$neg(someNestLens, innerBasicLens), $$double, record))));
              }));
        Jest.test("noneNest", (function (param) {
                return Jest.Expect.toEqual(0, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(noneNestLens, innerBasicLens), Lens$Rationale.over(Lens$Rationale.$great$great$neg(noneNestLens, innerBasicLens), $$double, record))));
              }));
        Jest.describe("list", (function (param) {
                Jest.test("head", (function (param) {
                        return Jest.Expect.toEqual(2, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.head), Lens$Rationale.optional(0)), Lens$Rationale.over(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.head), Lens$Rationale.optional(0)), $$double, record))));
                      }));
                Jest.test("tail", (function (param) {
                        return Jest.Expect.toEqual({
                                    hd: 4,
                                    tl: {
                                      hd: 6,
                                      tl: /* [] */0
                                    }
                                  }, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.tail), Lens$Rationale.optional({
                                                    hd: 0,
                                                    tl: /* [] */0
                                                  })), Lens$Rationale.over(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.tail), Lens$Rationale.optional({
                                                        hd: 0,
                                                        tl: /* [] */0
                                                      })), (function (param) {
                                                  return List.map($$double, param);
                                                }), record))));
                      }));
                return Jest.test("index", (function (param) {
                              return Jest.Expect.toEqual(4, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.index(1)), Lens$Rationale.optional(0)), Lens$Rationale.over(Lens$Rationale.$great$great$neg(Lens$Rationale.$great$great$neg(listLens, Lens$Rationale.index(1)), Lens$Rationale.optional(0)), $$double, record))));
                            }));
              }));
        Jest.test("dict", (function (param) {
                return Jest.Expect.toEqual(14, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(dictLens, aLens), Lens$Rationale.over(Lens$Rationale.$great$great$neg(dictLens, aLens), $$double, record))));
              }));
        return Jest.describe("tuple", (function (param) {
                      Jest.test("first", (function (param) {
                              return Jest.Expect.toEqual(16, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.first), Lens$Rationale.over(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.first), $$double, record))));
                            }));
                      return Jest.test("second", (function (param) {
                                    return Jest.Expect.toEqual(18, Jest.Expect.expect(Lens$Rationale.view(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.second), Lens$Rationale.over(Lens$Rationale.$great$great$neg(tupleLens, Lens$Rationale.second), $$double, record))));
                                  }));
                    }));
      }));

exports.record = record;
exports.basicLens = basicLens;
exports.innerBasicLens = innerBasicLens;
exports.nestLens = nestLens;
exports.someLens = someLens;
exports.noneLens = noneLens;
exports.someNestLens = someNestLens;
exports.noneNestLens = noneNestLens;
exports.listLens = listLens;
exports.emptyListLens = emptyListLens;
exports.dictLens = dictLens;
exports.aLens = aLens;
exports.tupleLens = tupleLens;
/* basicLens Not a pure module */
