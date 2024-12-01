// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2019_Day5$AdventOfCode from "./AOC2019_Day5.mjs";
import * as AOC2019_Day5_Data$AdventOfCode from "./AOC2019_Day5_Data.mjs";
import * as AOC2019_Day5_Data_Sample$AdventOfCode from "./AOC2019_Day5_Data_Sample.mjs";

Jest.describe("2019 Day5", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2019_Day5$AdventOfCode.solvePart1(AOC2019_Day5_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2019_Day5$AdventOfCode.solvePart1(AOC2019_Day5_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2019_Day5$AdventOfCode.solvePart2(AOC2019_Day5_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2019_Day5$AdventOfCode.solvePart2(AOC2019_Day5_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
});

let data = AOC2019_Day5_Data$AdventOfCode.data;

let sampleData = AOC2019_Day5_Data_Sample$AdventOfCode.data;

let solvePart1 = AOC2019_Day5$AdventOfCode.solvePart1;

let solvePart2 = AOC2019_Day5$AdventOfCode.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
