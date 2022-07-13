// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.bs.js";
import * as AOC2020_Day10$AdventOfCode from "./AOC2020_Day10.bs.js";
import * as AOC2020_Day10_Data$AdventOfCode from "./AOC2020_Day10_Data.bs.js";
import * as AOC2020_Day10_Data_Sample$AdventOfCode from "./AOC2020_Day10_Data_Sample.bs.js";

Jest.describe("2020 Day10", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2020_Day10$AdventOfCode.solvePart1(AOC2020_Day10_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 35);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2020_Day10$AdventOfCode.solvePart1(AOC2020_Day10_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 2470);
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2020_Day10$AdventOfCode.solvePart2(AOC2020_Day10_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(Jest.Expect.expect(result), 1973822685184.0);
                    }));
      }));

var data = AOC2020_Day10_Data$AdventOfCode.data;

var sampleData = AOC2020_Day10_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  
}
/*  Not a pure module */
