// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day11$AdventOfCode from "./AOC2021_Day11.mjs";
import * as AOC2021_Day11_Data$AdventOfCode from "./AOC2021_Day11_Data.mjs";
import * as AOC2021_Day11_Data_Sample$AdventOfCode from "./AOC2021_Day11_Data_Sample.mjs";

Jest.describe("2021 Day11", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day11$AdventOfCode.solvePart1(AOC2021_Day11_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1656);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day11$AdventOfCode.solvePart1(AOC2021_Day11_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1793);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day11$AdventOfCode.solvePart2(AOC2021_Day11_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 195);
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2021_Day11$AdventOfCode.solvePart2(AOC2021_Day11_Data$AdventOfCode.data);
                      return Jest.Expect.toEqual(Jest.Expect.expect(result), 247);
                    }));
      }));

var data = AOC2021_Day11_Data$AdventOfCode.data;

var sampleData = AOC2021_Day11_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  
}
/*  Not a pure module */