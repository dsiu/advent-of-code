// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.res.mjs";
import * as AOC2024_Day9 from "./AOC2024_Day9.res.mjs";
import * as AOC2024_Day9_Data from "./AOC2024_Day9_Data.res.mjs";
import * as AOC2024_Day9_Data_Sample from "./AOC2024_Day9_Data_Sample.res.mjs";

Jest.describe("2024 Day9", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2024_Day9.solvePart1(AOC2024_Day9_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1928n);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2024_Day9.solvePart1(AOC2024_Day9_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 6200294120911n);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2024_Day9.solvePart2(AOC2024_Day9_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2024_Day9.solvePart2(AOC2024_Day9_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
  });
});

let data = AOC2024_Day9_Data.data;

let sampleData = AOC2024_Day9_Data_Sample.data;

let solvePart1 = AOC2024_Day9.solvePart1;

let solvePart2 = AOC2024_Day9.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */