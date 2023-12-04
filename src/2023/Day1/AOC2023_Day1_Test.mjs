// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2023_Day1$AdventOfCode from "./AOC2023_Day1.mjs";
import * as AOC2023_Day1_Data$AdventOfCode from "./AOC2023_Day1_Data.mjs";
import * as AOC2023_Day1_Data_Sample$AdventOfCode from "./AOC2023_Day1_Data_Sample.mjs";

Jest.describe("2023 Day1", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2023_Day1$AdventOfCode.solvePart1(AOC2023_Day1_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 142);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2023_Day1$AdventOfCode.solvePart1(AOC2023_Day1_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 54159);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2023_Day1$AdventOfCode.solvePart2(AOC2023_Day1_Data_Sample$AdventOfCode.data2);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 281);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2023_Day1$AdventOfCode.solvePart2(AOC2023_Day1_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 53866);
              }));
      }));

var data = AOC2023_Day1_Data$AdventOfCode.data;

var sampleData = AOC2023_Day1_Data_Sample$AdventOfCode.data;

var sampleData2 = AOC2023_Day1_Data_Sample$AdventOfCode.data2;

var solvePart1 = AOC2023_Day1$AdventOfCode.solvePart1;

var solvePart2 = AOC2023_Day1$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  sampleData2 ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
