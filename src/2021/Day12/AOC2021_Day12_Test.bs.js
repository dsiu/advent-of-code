// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var AOC2021_Day12$AdventOfCode = require("./AOC2021_Day12.bs.js");
var AOC2021_Day12_Data$AdventOfCode = require("./AOC2021_Day12_Data.bs.js");
var AOC2021_Day12_Data_Sample$AdventOfCode = require("./AOC2021_Day12_Data_Sample.bs.js");

Jest.describe("2021 Day12", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(226, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Sample Data 0", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data0);
                return Jest.Expect.toEqual(10, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Sample Data 1", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(19, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(4338, Jest.Expect.expect(result));
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(3509, Jest.Expect.expect(result));
              }));
        Jest.test("Part 2 - Sample Data 0", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data0);
                return Jest.Expect.toEqual(36, Jest.Expect.expect(result));
              }));
        Jest.test("Part 2 - Sample Data 1", (function (param) {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(103, Jest.Expect.expect(result));
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(114189, Jest.Expect.expect(result));
                    }));
      }));

var data = AOC2021_Day12_Data$AdventOfCode.data;

var sampleData = AOC2021_Day12_Data_Sample$AdventOfCode.data;

var sampleData0 = AOC2021_Day12_Data_Sample$AdventOfCode.data0;

var sampleData1 = AOC2021_Day12_Data_Sample$AdventOfCode.data1;

exports.data = data;
exports.sampleData = sampleData;
exports.sampleData0 = sampleData0;
exports.sampleData1 = sampleData1;
/*  Not a pure module */
