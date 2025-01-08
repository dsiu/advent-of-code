// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2023_Day4 from "./AOC2023_Day4.mjs";
import * as AOC2023_Day4_Data from "./AOC2023_Day4_Data.mjs";
import * as AOC2023_Day4_Data_Sample from "./AOC2023_Day4_Data_Sample.mjs";

Jest.describe("2023 Day4", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2023_Day4.solvePart1(AOC2023_Day4_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 13);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2023_Day4.solvePart1(AOC2023_Day4_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 18619);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2023_Day4.solvePart2(AOC2023_Day4_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 30);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2023_Day4.solvePart2(AOC2023_Day4_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 8063216);
  });
});

let data = AOC2023_Day4_Data.data;

let sampleData = AOC2023_Day4_Data_Sample.data;

let solvePart1 = AOC2023_Day4.solvePart1;

let solvePart2 = AOC2023_Day4.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
