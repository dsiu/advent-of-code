// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2024_Day4 from "./AOC2024_Day4.mjs";
import * as AOC2024_Day4_Data from "./AOC2024_Day4_Data.mjs";
import * as AOC2024_Day4_Data_Sample from "./AOC2024_Day4_Data_Sample.mjs";

Jest.describe("2024 Day4", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2024_Day4.solvePart1(AOC2024_Day4_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 18);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2024_Day4.solvePart1(AOC2024_Day4_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2549);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2024_Day4.solvePart2(AOC2024_Day4_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 9);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2024_Day4.solvePart2(AOC2024_Day4_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2003);
  });
});

let data = AOC2024_Day4_Data.data;

let sampleData = AOC2024_Day4_Data_Sample.data;

let solvePart1 = AOC2024_Day4.solvePart1;

let solvePart2 = AOC2024_Day4.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
