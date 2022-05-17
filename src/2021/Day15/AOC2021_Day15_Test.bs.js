// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var AOC2021_Day15$AdventOfCode = require("./AOC2021_Day15.bs.js");
var AOC2021_Day15_Data$AdventOfCode = require("./AOC2021_Day15_Data.bs.js");
var AOC2021_Day15_Data_Sample$AdventOfCode = require("./AOC2021_Day15_Data_Sample.bs.js");

Jest.describe("2021 Day15", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day15$AdventOfCode.solvePart1(AOC2021_Day15_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(1, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day15$AdventOfCode.solvePart1(AOC2021_Day15_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(1, Jest.Expect.expect(result));
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day15$AdventOfCode.solvePart2(AOC2021_Day15_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(2, Jest.Expect.expect(result));
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2021_Day15$AdventOfCode.solvePart2(AOC2021_Day15_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(2, Jest.Expect.expect(result));
                    }));
      }));

var data = AOC2021_Day15_Data$AdventOfCode.data;

var sampleData = AOC2021_Day15_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2021_Day15$AdventOfCode.solvePart1;

var solvePart2 = AOC2021_Day15$AdventOfCode.solvePart2;

exports.data = data;
exports.sampleData = sampleData;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/*  Not a pure module */
