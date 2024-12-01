// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day10$AdventOfCode from "./AOC2020_Day10.mjs";
import * as AOC2020_Day10_Data$AdventOfCode from "./AOC2020_Day10_Data.mjs";
import * as AOC2020_Day10_Data_Sample$AdventOfCode from "./AOC2020_Day10_Data_Sample.mjs";

Jest.describe("2020 Day10", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day10$AdventOfCode.solvePart1(AOC2020_Day10_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 35);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day10$AdventOfCode.solvePart1(AOC2020_Day10_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2470);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day10$AdventOfCode.solvePart2(AOC2020_Day10_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1973822685184.0);
  });
});

let data = AOC2020_Day10_Data$AdventOfCode.data;

let sampleData = AOC2020_Day10_Data_Sample$AdventOfCode.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
