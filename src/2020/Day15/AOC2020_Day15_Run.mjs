// Generated by ReScript, PLEASE EDIT WITH CARE

import * as JS_Debug from "../../JS_Debug.mjs";
import * as AOC2020_Day15 from "./AOC2020_Day15.mjs";
import * as AOC2020_Day15_Data from "./AOC2020_Day15_Data.mjs";
import * as AOC2020_Day15_Data_Sample from "./AOC2020_Day15_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

let Log = JS_Debug.make("AOC2020_Day15_Run");

let part1 = AOC2020_Day15.solvePart1(AOC2020_Day15_Data.data);

Log.debug("Part 1 Result");

console.log(part1);

Log.debug("Part 1 Done");

let part2 = AOC2020_Day15.solvePart2(AOC2020_Day15_Data.data);

Log.debug("Part 2 Result");

console.log(part2);

Log.debug("Part 2 Done");

let data = AOC2020_Day15_Data.data;

let sampleData = AOC2020_Day15_Data_Sample.data;

export {
  log,
  data,
  sampleData,
  Log,
  part1,
  part2,
}
/* Log Not a pure module */
