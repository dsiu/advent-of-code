// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2021_Day1$AdventOfCode from "./AOC2021_Day1.mjs";
import * as AOC2021_Day1_Data$AdventOfCode from "./AOC2021_Day1_Data.mjs";
import * as AOC2021_Day1_Data_Sample$AdventOfCode from "./AOC2021_Day1_Data_Sample.mjs";

Jest.describe("2021 Day1", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day1$AdventOfCode.solvePart1(AOC2021_Day1_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 7);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2021_Day1$AdventOfCode.solvePart1(AOC2021_Day1_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1502);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2021_Day1$AdventOfCode.solvePart2(AOC2021_Day1_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1538);
  });
});

let data = AOC2021_Day1_Data$AdventOfCode.data;

let sampleData = AOC2021_Day1_Data_Sample$AdventOfCode.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
