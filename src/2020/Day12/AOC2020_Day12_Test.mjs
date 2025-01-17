// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day12$AdventOfCode from "./AOC2020_Day12.mjs";
import * as AOC2020_Day12_Data$AdventOfCode from "./AOC2020_Day12_Data.mjs";
import * as AOC2020_Day12_Data_Sample$AdventOfCode from "./AOC2020_Day12_Data_Sample.mjs";

Jest.describe("2020 Day12", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2020_Day12$AdventOfCode.solvePart1(AOC2020_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 25);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2020_Day12$AdventOfCode.solvePart1(AOC2020_Day12_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1010);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2020_Day12$AdventOfCode.solvePart2(AOC2020_Day12_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 286);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2020_Day12$AdventOfCode.solvePart2(AOC2020_Day12_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 52742);
              }));
      }));

var data = AOC2020_Day12_Data$AdventOfCode.data;

var sampleData = AOC2020_Day12_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
