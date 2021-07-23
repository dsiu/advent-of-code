// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var AOC2018_Day5$AdventOfCode = require("./AOC2018_Day5.bs.js");
var AOC2018_Day5_Data$AdventOfCode = require("./AOC2018_Day5_Data.bs.js");
var AOC2018_Day5_Data_Sample$AdventOfCode = require("./AOC2018_Day5_Data_Sample.bs.js");

var testCharArray = AOC2018_Day5_Data_Sample$AdventOfCode.data.split("");

var testCharList = Belt_List.fromArray(testCharArray);

var charArray = AOC2018_Day5_Data$AdventOfCode.data.split("");

var charList = Belt_List.fromArray(charArray);

Jest.describe("2018 Day5", (function (param) {
        Jest.describe("Part 1", (function (param) {
                Jest.test("fuse", (function (param) {
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
                        return Jest.Expect.toEqual([
                                    false,
                                    true,
                                    false,
                                    true,
                                    true,
                                    false,
                                    true
                                  ], Jest.Expect.expect(result));
                      }));
                Jest.test("Solve Part 1 - sampleData list", (function (param) {
                        var result = Belt_List.toArray(AOC2018_Day5$AdventOfCode.defuse(testCharList)).join("");
                        return Jest.Expect.toEqual("dabCBAcaDA", Jest.Expect.expect(result));
                      }));
                Jest.test("Solve Part 1 - sampleData array", (function (param) {
                        var result = AOC2018_Day5$AdventOfCode.defuse_array(testCharArray).join("");
                        return Jest.Expect.toEqual("dabCBAcaDA", Jest.Expect.expect(result));
                      }));
                return Jest.Skip.test("Solve Part 1 - Data array", (function (param) {
                              var result = AOC2018_Day5$AdventOfCode.defuse_array(charArray).join("").length;
                              var expected = AOC2018_Day5_Data$AdventOfCode.result.length;
                              return Jest.Expect.toEqual(expected, Jest.Expect.expect(result));
                            }));
              }));
        return Jest.describe("Part 2", (function (param) {
                      Jest.test("notIsLetterAndUpper", (function (param) {
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
                              return Jest.Expect.toEqual(expected, Jest.Expect.expect(result));
                            }));
                      Jest.test("Solve Part 2 - sampleData", (function (param) {
                              var result = AOC2018_Day5$AdventOfCode.solvePart2(AOC2018_Day5$AdventOfCode.aTod, testCharArray);
                              return Jest.Expect.toEqual(4, Jest.Expect.expect(result));
                            }));
                      return Jest.Skip.test("Solve Part 2 - Data", (function (param) {
                                    var result = AOC2018_Day5$AdventOfCode.solvePart2(AOC2018_Day5$AdventOfCode.aToz, charArray);
                                    return Jest.Expect.toEqual(4282, Jest.Expect.expect(result));
                                  }));
                    }));
      }));

var data = AOC2018_Day5_Data$AdventOfCode.data;

var sampleData = AOC2018_Day5_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
exports.testCharArray = testCharArray;
exports.testCharList = testCharList;
exports.charArray = charArray;
exports.charList = charList;
/* testCharArray Not a pure module */