// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2024_Day7$AdventOfCode from "./AOC2024_Day7.mjs";
import * as AOC2024_Day7_Data$AdventOfCode from "./AOC2024_Day7_Data.mjs";
import * as AOC2024_Day7_Data_Sample$AdventOfCode from "./AOC2024_Day7_Data_Sample.mjs";

Jest.describe("2024 Day7", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2024_Day7$AdventOfCode.solvePart1(AOC2024_Day7_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 3749n);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2024_Day7$AdventOfCode.solvePart1(AOC2024_Day7_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 850435817339n);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2024_Day7$AdventOfCode.solvePart2(AOC2024_Day7_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 11387n);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2024_Day7$AdventOfCode.solvePart2(AOC2024_Day7_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 104824810233437n);
  });
});

let data = AOC2024_Day7_Data$AdventOfCode.data;

let sampleData = AOC2024_Day7_Data_Sample$AdventOfCode.data;

let solvePart1 = AOC2024_Day7$AdventOfCode.solvePart1;

let solvePart2 = AOC2024_Day7$AdventOfCode.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
