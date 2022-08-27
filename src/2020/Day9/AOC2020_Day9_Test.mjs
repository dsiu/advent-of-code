// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day9$AdventOfCode from "./AOC2020_Day9.mjs";
import * as AOC2020_Day9_Data$AdventOfCode from "./AOC2020_Day9_Data.mjs";
import * as AOC2020_Day9_Data_Sample$AdventOfCode from "./AOC2020_Day9_Data_Sample.mjs";

Jest.describe("2020 Day9", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2020_Day9$AdventOfCode.solvePart1(AOC2020_Day9_Data_Sample$AdventOfCode.data, 5);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 127);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2020_Day9$AdventOfCode.solvePart1(AOC2020_Day9_Data$AdventOfCode.data, 25);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 31161678);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2020_Day9$AdventOfCode.solvePart2(AOC2020_Day9_Data_Sample$AdventOfCode.data, 5);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 62);
              }));
        Jest.test("Part 2 - Solve", (function (param) {
                var result = AOC2020_Day9$AdventOfCode.solvePart2(AOC2020_Day9_Data$AdventOfCode.data, 25);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 5453868);
              }));
      }));

var data = AOC2020_Day9_Data$AdventOfCode.data;

var sampleData = AOC2020_Day9_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
