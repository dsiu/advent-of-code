// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day12$AdventOfCode from "./AOC2021_Day12.mjs";
import * as AOC2021_Day12_Data$AdventOfCode from "./AOC2021_Day12_Data.mjs";
import * as AOC2021_Day12_Data_Sample$AdventOfCode from "./AOC2021_Day12_Data_Sample.mjs";

Jest.describe("2021 Day12", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 226);
              }));
        Jest.test("Part 1 - Sample Data 0", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data0);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 10);
              }));
        Jest.test("Part 1 - Sample Data 1", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 19);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart1(AOC2021_Day12_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 4338);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 3509);
              }));
        Jest.test("Part 2 - Sample Data 0", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data0);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 36);
              }));
        Jest.test("Part 2 - Sample Data 1", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 103);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2021_Day12$AdventOfCode.solvePart2(AOC2021_Day12_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 114189);
              }));
      }));

var data = AOC2021_Day12_Data$AdventOfCode.data;

var sampleData = AOC2021_Day12_Data_Sample$AdventOfCode.data;

var sampleData0 = AOC2021_Day12_Data_Sample$AdventOfCode.data0;

var sampleData1 = AOC2021_Day12_Data_Sample$AdventOfCode.data1;

export {
  data ,
  sampleData ,
  sampleData0 ,
  sampleData1 ,
}
/*  Not a pure module */
