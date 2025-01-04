
let log = Console.log
let data = AOC2024_Day3_Data.data
let sampleData1 = AOC2024_Day3_Data_Sample.data1
let sampleData2 = AOC2024_Day3_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2024_Day3)

Console.time("Part 1")

//let part1 = solvePart1(sampleData1)
let part1 = solvePart1(data)

"Part 1 Result"->log
part1->log
Console.timeEnd("Part 1")

"----------"->log

Console.time("Part 2")

//let part2 = solvePart2(sampleData2)
let part2 = solvePart2(data)

"Part 2 Result"->log
part2->log
Console.timeEnd("Part 2")
