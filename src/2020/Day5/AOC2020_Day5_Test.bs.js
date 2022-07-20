// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/rescript-jest/src/jest.bs.js");
var AOC2020_Day5$AdventOfCode = require("./AOC2020_Day5.bs.js");
var AOC2020_Day5_Data$AdventOfCode = require("./AOC2020_Day5_Data.bs.js");
var AOC2020_Day5_Data_Sample$AdventOfCode = require("./AOC2020_Day5_Data_Sample.bs.js");

Jest.describe("2020 Day5", (function (param) {
        Jest.test("Part 1 - Test Data", (function (param) {
                var result = AOC2020_Day5$AdventOfCode.solvePart1(AOC2020_Day5_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 820);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2020_Day5$AdventOfCode.solvePart1(AOC2020_Day5_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 908);
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2020_Day5$AdventOfCode.solvePart2(AOC2020_Day5_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(Jest.Expect.expect(result), 619);
                    }));
      }));

var data = AOC2020_Day5_Data$AdventOfCode.data;

var sampleData = AOC2020_Day5_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
/*  Not a pure module */
