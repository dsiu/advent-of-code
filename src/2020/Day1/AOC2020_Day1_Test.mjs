// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day1 from "./AOC2020_Day1.mjs";
import * as AOC2020_Day1_Data from "./AOC2020_Day1_Data.mjs";
import * as AOC2020_Day1_Data_Sample from "./AOC2020_Day1_Data_Sample.mjs";

Jest.describe("2020 Day1", () => {
  Jest.test("Part 1 - Test Data", () => {
    let result = AOC2020_Day1.solvePart1(AOC2020_Day1_Data_Sample.data);
    let expected = [
      514579,
      514579
    ];
    return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day1.solvePart1(AOC2020_Day1_Data.data);
    let expected = [
      1013211,
      1013211
    ];
    return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day1.solvePart2(AOC2020_Day1_Data.data);
    let expected = [
      13891280,
      13891280,
      13891280
    ];
    return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
  });
});

let data = AOC2020_Day1_Data.data;

let sampleData = AOC2020_Day1_Data_Sample.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
