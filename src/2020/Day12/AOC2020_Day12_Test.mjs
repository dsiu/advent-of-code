// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2020_Day12 from "./AOC2020_Day12.mjs";
import * as AOC2020_Day12_Data from "./AOC2020_Day12_Data.mjs";
import * as AOC2020_Day12_Data_Sample from "./AOC2020_Day12_Data_Sample.mjs";

Jest.describe("2020 Day12", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day12.solvePart1(AOC2020_Day12_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 25);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day12.solvePart1(AOC2020_Day12_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1010);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2020_Day12.solvePart2(AOC2020_Day12_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 286);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day12.solvePart2(AOC2020_Day12_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 52742);
  });
});

let data = AOC2020_Day12_Data.data;

let sampleData = AOC2020_Day12_Data_Sample.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
