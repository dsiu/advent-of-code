// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2024_Day8$AdventOfCode from "./AOC2024_Day8.mjs";
import * as AOC2024_Day8_Data$AdventOfCode from "./AOC2024_Day8_Data.mjs";
import * as AOC2024_Day8_Data_Sample$AdventOfCode from "./AOC2024_Day8_Data_Sample.mjs";

Jest.describe("2024 Day8", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2024_Day8$AdventOfCode.solvePart1(AOC2024_Day8_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 14);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2024_Day8$AdventOfCode.solvePart1(AOC2024_Day8_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 265);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2024_Day8$AdventOfCode.solvePart2(AOC2024_Day8_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 34);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2024_Day8$AdventOfCode.solvePart2(AOC2024_Day8_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 962);
  });
});

let data = AOC2024_Day8_Data$AdventOfCode.data;

let sampleData = AOC2024_Day8_Data_Sample$AdventOfCode.data;

let solvePart1 = AOC2024_Day8$AdventOfCode.solvePart1;

let solvePart2 = AOC2024_Day8$AdventOfCode.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */