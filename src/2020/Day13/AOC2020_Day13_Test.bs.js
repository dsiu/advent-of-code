// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var AOC2020_Day13$AdventOfCode = require("./AOC2020_Day13.bs.js");
var AOC2020_Day13_Data$AdventOfCode = require("./AOC2020_Day13_Data.bs.js");
var AOC2020_Day13_Data_Sample$AdventOfCode = require("./AOC2020_Day13_Data_Sample.bs.js");

Jest.describe("2020 Day13", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2020_Day13$AdventOfCode.solvePart1(AOC2020_Day13_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(1, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2020_Day13$AdventOfCode.solvePart1(AOC2020_Day13_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(1, Jest.Expect.expect(result));
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2020_Day13$AdventOfCode.solvePart2(AOC2020_Day13_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(2, Jest.Expect.expect(result));
                    }));
      }));

var data = AOC2020_Day13_Data$AdventOfCode.data;

var sampleData = AOC2020_Day13_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
/*  Not a pure module */
