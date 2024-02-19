// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day6$AdventOfCode from "./AOC2020_Day6.mjs";
import * as AOC2020_Day6_Data$AdventOfCode from "./AOC2020_Day6_Data.mjs";
import * as AOC2020_Day6_Data_Sample$AdventOfCode from "./AOC2020_Day6_Data_Sample.mjs";

Jest.describe("2020 Day6", (function () {
        Jest.test("Part 1 - Test Data", (function () {
                var result = AOC2020_Day6$AdventOfCode.solvePart1(AOC2020_Day6_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 11);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2020_Day6$AdventOfCode.solvePart1(AOC2020_Day6_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 6457);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2020_Day6$AdventOfCode.solvePart2(AOC2020_Day6_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 3260);
              }));
      }));

var data = AOC2020_Day6_Data$AdventOfCode.data;

var sampleData = AOC2020_Day6_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
