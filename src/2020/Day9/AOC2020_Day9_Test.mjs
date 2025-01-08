// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day9 from "./AOC2020_Day9.mjs";
import * as AOC2020_Day9_Data from "./AOC2020_Day9_Data.mjs";
import * as AOC2020_Day9_Data_Sample from "./AOC2020_Day9_Data_Sample.mjs";

Jest.describe("2020 Day9", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day9.solvePart1(AOC2020_Day9_Data_Sample.data, 5);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 127);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day9.solvePart1(AOC2020_Day9_Data.data, 25);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 31161678);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2020_Day9.solvePart2(AOC2020_Day9_Data_Sample.data, 5);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 62);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day9.solvePart2(AOC2020_Day9_Data.data, 25);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 5453868);
  });
});

let data = AOC2020_Day9_Data.data;

let sampleData = AOC2020_Day9_Data_Sample.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
