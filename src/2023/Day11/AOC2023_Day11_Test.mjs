// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2023_Day11$AdventOfCode from "./AOC2023_Day11.mjs";
import * as AOC2023_Day11_Data$AdventOfCode from "./AOC2023_Day11_Data.mjs";
import * as AOC2023_Day11_Data_Sample$AdventOfCode from "./AOC2023_Day11_Data_Sample.mjs";

Jest.describe("2023 Day11", (function () {
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2023_Day11$AdventOfCode.solvePart1(AOC2023_Day11_Data_Sample$AdventOfCode.data);
                var expected = BigInt(374);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2023_Day11$AdventOfCode.solvePart1(AOC2023_Day11_Data$AdventOfCode.data);
                var expected = BigInt(9445168);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 2 - Sample Data", (function () {
                var result = AOC2023_Day11$AdventOfCode.solvePart2(AOC2023_Day11_Data_Sample$AdventOfCode.data);
                var expected = BigInt(82000210);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2023_Day11$AdventOfCode.solvePart2(AOC2023_Day11_Data$AdventOfCode.data);
                var expected = BigInt("742305960572");
                return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
              }));
      }));

var data = AOC2023_Day11_Data$AdventOfCode.data;

var sampleData = AOC2023_Day11_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2023_Day11$AdventOfCode.solvePart1;

var solvePart2 = AOC2023_Day11$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
