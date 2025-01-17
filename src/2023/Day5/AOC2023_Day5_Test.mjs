// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2023_Day5$AdventOfCode from "./AOC2023_Day5.mjs";
import * as AOC2023_Day5_Data$AdventOfCode from "./AOC2023_Day5_Data.mjs";
import * as AOC2023_Day5_Data_Sample$AdventOfCode from "./AOC2023_Day5_Data_Sample.mjs";

Jest.describe("2023 Day5", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2023_Day5$AdventOfCode.solvePart1(AOC2023_Day5_Data_Sample$AdventOfCode.data);
                var expected = BigInt(35);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2023_Day5$AdventOfCode.solvePart1(AOC2023_Day5_Data$AdventOfCode.data);
                var expected = BigInt("227653707");
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2023_Day5$AdventOfCode.solvePart2(AOC2023_Day5_Data_Sample$AdventOfCode.data);
                var expected = BigInt(46);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2023_Day5$AdventOfCode.solvePart2(AOC2023_Day5_Data$AdventOfCode.data);
                var expected = BigInt("78775051");
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
      }));

var data = AOC2023_Day5_Data$AdventOfCode.data;

var sampleData = AOC2023_Day5_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2023_Day5$AdventOfCode.solvePart1;

var solvePart2 = AOC2023_Day5$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
