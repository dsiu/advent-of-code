let log = Js.Console.log
let data = AOC2021_Day19_Data.data
let sampleData = AOC2021_Day19_Data_Sample.data
let {solvePart1, solvePart2} = module(AOC2021_Day19)

let part1 = solvePart1(sampleData)
//let part1 = solvePart1(data)
"Part 1 Result"->log
part1->log
//let part2 = solvePart2(sampleData)
let part2 = solvePart2(data)
"Part 2 Result"->log
part2->log
