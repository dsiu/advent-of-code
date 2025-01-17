// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as AOC2018_Day5$AdventOfCode from "./AOC2018_Day5.mjs";
import * as AOC2018_Day5_Data$AdventOfCode from "./AOC2018_Day5_Data.mjs";
import * as AOC2018_Day5_Data_Sample$AdventOfCode from "./AOC2018_Day5_Data_Sample.mjs";

var testCharArray = AOC2018_Day5_Data_Sample$AdventOfCode.data.split("");

var testCharList = Belt_List.fromArray(testCharArray);

var charArray = AOC2018_Day5_Data$AdventOfCode.data.split("");

var charList = Belt_List.fromArray(charArray);

Jest.describe("2018 Day5", (function () {
        Jest.describe("Part 1", (function () {
                Jest.test("fuse", (function () {
                        var result_0 = AOC2018_Day5$AdventOfCode.fuse([
                              "a",
                              "b"
                            ]);
                        var result_1 = AOC2018_Day5$AdventOfCode.fuse([
                              "a",
                              "A"
                            ]);
                        var result_2 = AOC2018_Day5$AdventOfCode.fuse([
                              "a",
                              "B"
                            ]);
                        var result_3 = AOC2018_Day5$AdventOfCode.fuse([
                              "c",
                              "C"
                            ]);
                        var result_4 = AOC2018_Day5$AdventOfCode.fuse([
                              "D",
                              "d"
                            ]);
                        var result_5 = AOC2018_Day5$AdventOfCode.fuse([
                              "E",
                              "Z"
                            ]);
                        var result_6 = AOC2018_Day5$AdventOfCode.fuse([
                              "A",
                              "a"
                            ]);
                        var result = [
                          result_0,
                          result_1,
                          result_2,
                          result_3,
                          result_4,
                          result_5,
                          result_6
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), [
                                    false,
                                    true,
                                    false,
                                    true,
                                    true,
                                    false,
                                    true
                                  ]);
                      }));
                Jest.test("Solve Part 1 - sampleData list", (function () {
                        var result = (function (__x) {
                              return __x.join("");
                            })(Belt_List.toArray(AOC2018_Day5$AdventOfCode.defuse(testCharList)));
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), "dabCBAcaDA");
                      }));
                Jest.test("Solve Part 1 - sampleData array", (function () {
                        var result = (function (__x) {
                              return __x.join("");
                            })(AOC2018_Day5$AdventOfCode.defuse_array(testCharArray));
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), "dabCBAcaDA");
                      }));
                Jest.Skip.test("Solve Part 1 - Data array", (function () {
                        var result = (function (__x) {
                              return __x.join("");
                            })(AOC2018_Day5$AdventOfCode.defuse_array(charArray)).length;
                        var expected = AOC2018_Day5_Data$AdventOfCode.result.length;
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
              }));
        Jest.describe("Part 2", (function () {
                Jest.test("notIsLetterAndUpper", (function () {
                        var result_0 = Belt_Array.keep(testCharArray, (function (param) {
                                return AOC2018_Day5$AdventOfCode.notIsLetterAndUpper("a", param);
                              }));
                        var result_1 = Belt_Array.keep(testCharArray, (function (param) {
                                return AOC2018_Day5$AdventOfCode.notIsLetterAndUpper("b", param);
                              }));
                        var result = [
                          result_0,
                          result_1
                        ];
                        var expected_0 = "dbcCCBcCcD".split("");
                        var expected_1 = "daAcCaCAcCcaDA".split("");
                        var expected = [
                          expected_0,
                          expected_1
                        ];
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("Solve Part 2 - sampleData", (function () {
                        var result = AOC2018_Day5$AdventOfCode.solvePart2(AOC2018_Day5$AdventOfCode.aTod, testCharArray);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), 4);
                      }));
                Jest.Skip.test("Solve Part 2 - Data", (function () {
                        var result = AOC2018_Day5$AdventOfCode.solvePart2(AOC2018_Day5$AdventOfCode.aToz, charArray);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), 4282);
                      }));
              }));
      }));

var data = AOC2018_Day5_Data$AdventOfCode.data;

var sampleData = AOC2018_Day5_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  testCharArray ,
  testCharList ,
  charArray ,
  charList ,
}
/* testCharArray Not a pure module */
