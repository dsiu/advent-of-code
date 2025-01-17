// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2022_Day1$AdventOfCode from "./AOC2022_Day1.mjs";
import * as AOC2022_Day1_Data$AdventOfCode from "./AOC2022_Day1_Data.mjs";
import * as AOC2022_Day1_Data_Sample$AdventOfCode from "./AOC2022_Day1_Data_Sample.mjs";

Jest.describe("2022 Day1", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2022_Day1$AdventOfCode.solvePart1(AOC2022_Day1_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 24000);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2022_Day1$AdventOfCode.solvePart1(AOC2022_Day1_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 64929);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2022_Day1$AdventOfCode.solvePart2(AOC2022_Day1_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 45000);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2022_Day1$AdventOfCode.solvePart2(AOC2022_Day1_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 193697);
              }));
      }));

var data = AOC2022_Day1_Data$AdventOfCode.data;

var sampleData = AOC2022_Day1_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2022_Day1$AdventOfCode.solvePart1;

var solvePart2 = AOC2022_Day1$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
