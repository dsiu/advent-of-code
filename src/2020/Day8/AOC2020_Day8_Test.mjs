// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day8$AdventOfCode from "./AOC2020_Day8.mjs";
import * as AOC2020_Day8_Data$AdventOfCode from "./AOC2020_Day8_Data.mjs";
import * as AOC2020_Day8_Data_Sample$AdventOfCode from "./AOC2020_Day8_Data_Sample.mjs";

Jest.describe("2020 Day8", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2020_Day8$AdventOfCode.solvePart1(AOC2020_Day8_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 5);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2020_Day8$AdventOfCode.solvePart1(AOC2020_Day8_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1394);
              }));
        Jest.test("Part 2 - Solve", (function (param) {
                var result = AOC2020_Day8$AdventOfCode.solvePart2(AOC2020_Day8_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1626);
              }));
      }));

var data = AOC2020_Day8_Data$AdventOfCode.data;

var sampleData = AOC2020_Day8_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
