// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.bs.js";
import * as AOC2021_Day10$AdventOfCode from "./AOC2021_Day10.bs.js";
import * as AOC2021_Day10_Data$AdventOfCode from "./AOC2021_Day10_Data.bs.js";
import * as AOC2021_Day10_Data_Sample$AdventOfCode from "./AOC2021_Day10_Data_Sample.bs.js";

Jest.describe("2021 Day10", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day10$AdventOfCode.solvePart1(AOC2021_Day10_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 26397);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day10$AdventOfCode.solvePart1(AOC2021_Day10_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 392139);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day10$AdventOfCode.solvePart2(AOC2021_Day10_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), "288957");
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2021_Day10$AdventOfCode.solvePart2(AOC2021_Day10_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(Jest.Expect.expect(result), "4001832844");
                    }));
      }));

var data = AOC2021_Day10_Data$AdventOfCode.data;

var sampleData = AOC2021_Day10_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  
}
/*  Not a pure module */
