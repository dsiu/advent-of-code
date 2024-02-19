// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day13$AdventOfCode from "./AOC2021_Day13.mjs";
import * as AOC2021_Day13_Data$AdventOfCode from "./AOC2021_Day13_Data.mjs";
import * as AOC2021_Day13_Data_Sample$AdventOfCode from "./AOC2021_Day13_Data_Sample.mjs";

Jest.describe("2021 Day13", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day13$AdventOfCode.solvePart1(AOC2021_Day13_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 17);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2021_Day13$AdventOfCode.solvePart1(AOC2021_Day13_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 710);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day13$AdventOfCode.solvePart2(AOC2021_Day13_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 16);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2021_Day13$AdventOfCode.solvePart2(AOC2021_Day13_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 97);
              }));
      }));

var data = AOC2021_Day13_Data$AdventOfCode.data;

var sampleData = AOC2021_Day13_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2021_Day13$AdventOfCode.solvePart1;

var solvePart2 = AOC2021_Day13$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
