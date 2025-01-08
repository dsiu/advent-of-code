// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2024_DayX from "./AOC2024_DayX.mjs";
import * as AOC2024_DayX_Data from "./AOC2024_DayX_Data.mjs";
import * as AOC2024_DayX_Data_Sample from "./AOC2024_DayX_Data_Sample.mjs";

Jest.describe("2024 DayX", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2024_DayX.solvePart1(AOC2024_DayX_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2024_DayX.solvePart1(AOC2024_DayX_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2024_DayX.solvePart2(AOC2024_DayX_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2024_DayX.solvePart2(AOC2024_DayX_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
});

let data = AOC2024_DayX_Data.data;

let sampleData = AOC2024_DayX_Data_Sample.data;

let solvePart1 = AOC2024_DayX.solvePart1;

let solvePart2 = AOC2024_DayX.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
