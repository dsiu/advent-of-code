// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day2$AdventOfCode from "./AOC2021_Day2.mjs";
import * as AOC2021_Day2_Data$AdventOfCode from "./AOC2021_Day2_Data.mjs";
import * as AOC2021_Day2_Data_Sample$AdventOfCode from "./AOC2021_Day2_Data_Sample.mjs";

Jest.describe("2021 Day2", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day2$AdventOfCode.solvePart1(AOC2021_Day2_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 150);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2021_Day2$AdventOfCode.solvePart1(AOC2021_Day2_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1804520);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day2$AdventOfCode.solvePart2(AOC2021_Day2_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 900);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2021_Day2$AdventOfCode.solvePart2(AOC2021_Day2_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1971095320);
              }));
      }));

var data = AOC2021_Day2_Data$AdventOfCode.data;

var sampleData = AOC2021_Day2_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
