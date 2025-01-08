// Generated by ReScript, PLEASE EDIT WITH CARE

import * as AOC2023_Day1 from "./AOC2023_Day1.mjs";
import * as AOC2023_Day1_Data from "./AOC2023_Day1_Data.mjs";
import * as AOC2023_Day1_Data_Sample from "./AOC2023_Day1_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

console.time("Part 1");

let part1 = AOC2023_Day1.solvePart1(AOC2023_Day1_Data.data);

console.log("Part 1 Result");

console.log(part1);

console.timeEnd("Part 1");

console.log("----------");

console.time("Part 2");

let part2 = AOC2023_Day1.solvePart2(AOC2023_Day1_Data.data);

console.log("Part 2 Result");

console.log(part2);

console.timeEnd("Part 2");

let data = AOC2023_Day1_Data.data;

let sampleData = AOC2023_Day1_Data_Sample.data;

let sampleData2 = AOC2023_Day1_Data_Sample.data2;

let solvePart1 = AOC2023_Day1.solvePart1;

let solvePart2 = AOC2023_Day1.solvePart2;

export {
  log,
  data,
  sampleData,
  sampleData2,
  solvePart1,
  solvePart2,
  part1,
  part2,
}
/*  Not a pure module */
