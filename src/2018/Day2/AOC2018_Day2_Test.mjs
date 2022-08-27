// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";
import * as AOC2018_Day2$AdventOfCode from "./AOC2018_Day2.mjs";
import * as AOC2018_Day2_Data$AdventOfCode from "./AOC2018_Day2_Data.mjs";

Jest.describe("2018 Day2", (function (param) {
        var lines = AOC2018_Day2_Data$AdventOfCode.data.split("\n");
        var len = lines.length;
        Jest.describe("Part1", (function (param) {
                Jest.test("number of lines", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(len), 250);
                      }));
                Jest.test("first line", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(Caml_array.get(lines, 0)), "crruafyzloguvxwctqmphenbkd");
                      }));
                Jest.test("last line", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(Caml_array.get(lines, len - 1 | 0)), "hrijafyzloguvxectqmpheybkd");
                      }));
                var test_string = "abbbcc";
                Jest.test("string to char[]", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.string_to_charStr(test_string)), [
                                    "a",
                                    "b",
                                    "b",
                                    "b",
                                    "c",
                                    "c"
                                  ]);
                      }));
                Jest.test("char freq", (function (param) {
                        var expected = Belt_MapString.set(Belt_MapString.set(Belt_MapString.set(undefined, "a", 1), "b", 3), "c", 2);
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.char_freq(AOC2018_Day2$AdventOfCode.string_to_charStr(test_string))), expected);
                      }));
                Jest.test("char freq match", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.n_char_matched_freq(3, test_string)), 1);
                      }));
                Jest.test("threeTimesCounter", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.threeTimesCounter("aabbbccccccddddd")), 1);
                      }));
              }));
        Jest.describe("Part2", (function (param) {
                Jest.test("diffOfTwoCharStr", (function (param) {
                        var expected = [
                          {
                            TAG: /* Match */0,
                            _0: "a"
                          },
                          {
                            TAG: /* Match */0,
                            _0: "b"
                          },
                          {
                            TAG: /* Match */0,
                            _0: "c"
                          },
                          {
                            TAG: /* Match */0,
                            _0: "d"
                          },
                          {
                            TAG: /* Match */0,
                            _0: "e"
                          },
                          {
                            TAG: /* NotMatch */1,
                            _0: "f",
                            _1: "g"
                          }
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.diffOfTwoCharStr("abcdef", "abcdeg")), expected);
                      }));
                Jest.test("count true", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.countTrue(AOC2018_Day2$AdventOfCode.diffOfTwoCharStr("abcdef", "abcdeg"))), 5);
                      }));
                Jest.test("count false", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.countTrue(AOC2018_Day2$AdventOfCode.diffOfTwoCharStr("abcdef", "abcdeg"))), 5);
                      }));
                Jest.test("diff by 1 char strs", (function (param) {
                        return Jest.Expect.toBe(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.isDiffBy1(AOC2018_Day2$AdventOfCode.diffOfTwoCharStr("abcdef", "abcdeg"))), true);
                      }));
                Jest.test("diff by 5 char strs", (function (param) {
                        return Jest.Expect.toBe(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.isDiffBy5(AOC2018_Day2$AdventOfCode.diffOfTwoCharStr("zzzzzz", "zabcde"))), true);
                      }));
                Jest.test("run part2 test", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.runDay2Part2([
                                            "abcdef",
                                            "abcdee"
                                          ])), [
                                    "abcde",
                                    "abcde"
                                  ]);
                      }));
                Jest.test("run part2", (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(AOC2018_Day2$AdventOfCode.runDay2Part2(AOC2018_Day2_Data$AdventOfCode.data.split("\n"))), [
                                    "srijafjzloguvlntqmphenbkd",
                                    "srijafjzloguvlntqmphenbkd"
                                  ]);
                      }));
              }));
      }));

export {
  
}
/*  Not a pure module */
