// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.res.mjs";
import * as AOC2021_Day19 from "./AOC2021_Day19.res.mjs";
import * as AOC2021_Day19_Data from "./AOC2021_Day19_Data.res.mjs";
import * as AOC2021_Day19_Data_Sample from "./AOC2021_Day19_Data_Sample.res.mjs";

Jest.describe("2021 Day19", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day19.solvePart1(AOC2021_Day19_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 79);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2021_Day19.solvePart1(AOC2021_Day19_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 394);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day19.solvePart2(AOC2021_Day19_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 3621);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2021_Day19.solvePart2(AOC2021_Day19_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 12304);
  });
});

let data = AOC2021_Day19_Data.data;

let sampleData = AOC2021_Day19_Data_Sample.data;

let solvePart1 = AOC2021_Day19.solvePart1;

let solvePart2 = AOC2021_Day19.solvePart2;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
}
/*  Not a pure module */