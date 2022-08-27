// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day3$AdventOfCode from "./AOC2021_Day3.mjs";
import * as AOC2021_Day3_Data$AdventOfCode from "./AOC2021_Day3_Data.mjs";
import * as AOC2021_Day3_Data_Sample$AdventOfCode from "./AOC2021_Day3_Data_Sample.mjs";

Jest.describe("2021 Day3", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day3$AdventOfCode.solvePart1(AOC2021_Day3_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 198);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day3$AdventOfCode.solvePart1(AOC2021_Day3_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1307354);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day3$AdventOfCode.solvePart2(AOC2021_Day3_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 230);
              }));
        Jest.test("Part 2 - Solve", (function (param) {
                var result = AOC2021_Day3$AdventOfCode.solvePart2(AOC2021_Day3_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 482500);
              }));
      }));

var data = AOC2021_Day3_Data$AdventOfCode.data;

var sampleData = AOC2021_Day3_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
