// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var Day4$AdventOfCode = require("../../../src/2018/Day4/Day4.bs.js");
var Day4_Data$AdventOfCode = require("../../../src/2018/Day4/Day4_Data.bs.js");
var Day4_Data_Test$AdventOfCode = require("../../../src/2018/Day4/Day4_Data_Test.bs.js");

Jest.describe("2018 Day4", (function (param) {
        Jest.describe("Part 1", (function (param) {
                Jest.test("Solve Part 1 - testData", (function (param) {
                        var result = Day4$AdventOfCode.solvePart1(Day4_Data_Test$AdventOfCode.data);
                        return Jest.Expect.toEqual(240, Jest.Expect.expect(result));
                      }));
                return Jest.test("Solve Part 1 - Data", (function (param) {
                              var result = Day4$AdventOfCode.solvePart1(Day4_Data$AdventOfCode.data);
                              return Jest.Expect.toEqual(48680, Jest.Expect.expect(result));
                            }));
              }));
        return Jest.describe("Part 2", (function (param) {
                      Jest.test("Solve Part 2 - testData", (function (param) {
                              var result = Day4$AdventOfCode.solvePart2(Day4_Data_Test$AdventOfCode.data);
                              return Jest.Expect.toEqual(4455, Jest.Expect.expect(result));
                            }));
                      return Jest.test("Solve Part 2 - Data", (function (param) {
                                    var result = Day4$AdventOfCode.solvePart2(Day4_Data$AdventOfCode.data);
                                    return Jest.Expect.toEqual(94826, Jest.Expect.expect(result));
                                  }));
                    }));
      }));

var data = Day4_Data$AdventOfCode.data;

var testData = Day4_Data_Test$AdventOfCode.data;

exports.data = data;
exports.testData = testData;
/*  Not a pure module */
