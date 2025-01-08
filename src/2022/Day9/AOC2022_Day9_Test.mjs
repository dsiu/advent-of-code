// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2022_Day9 from "./AOC2022_Day9.mjs";
import * as AOC2022_Day9_Data from "./AOC2022_Day9_Data.mjs";
import * as AOC2022_Day9_Data_Sample from "./AOC2022_Day9_Data_Sample.mjs";

Jest.describe("2022 Day9", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2022_Day9.solvePart1(AOC2022_Day9_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 13);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2022_Day9.solvePart1(AOC2022_Day9_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 5874);
  });
  Jest.test("Part 2 - Sample Data 1", () => {
    let result = AOC2022_Day9.solvePart2(AOC2022_Day9_Data_Sample.data1);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 36);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2022_Day9.solvePart2(AOC2022_Day9_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2467);
  });
});

let data = AOC2022_Day9_Data.data;

let sampleData = AOC2022_Day9_Data_Sample.data;

let sampleData1 = AOC2022_Day9_Data_Sample.data1;

let solvePart1 = AOC2022_Day9.solvePart1;

let solvePart2 = AOC2022_Day9.solvePart2;

export {
  data,
  sampleData,
  sampleData1,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */
